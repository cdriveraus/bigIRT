// The block-diagonal Fisher metric for the Laplace optimiser, in one parallel
// pass over persons.
//
// The optimiser (R/laplaceOptimizer.R) shapes L-BFGS with the Fisher
// information of the marginal. Its joint (items + persons) form is
// I_ii = V'V, I_ip = V'W, I_pp = W'W + P0 per person, where V and W are the
// response derivatives against item and person parameters weighted by
// 1/sqrt(p(1-p)); the marginal's is the Schur complement
//
//   F = V'V - C' Ipp^-1 C + priors,   C = W'V (+ the ability-mean term).
//
// The metric keeps F's blocks for each item's own parameters and the exact
// block for the parameters shared across items (G: shared covariate
// coefficients, person covariates, mean hyperparameters, the ability mean),
// and drops the data's coupling between blocks. Both are principal
// submatrices of a positive semidefinite matrix, so the metric is too. The
// prior's coupling of item parameters to shared ones (each to its mean
// hyperparameter) is added back in R, as a border applied through the shared
// block's Schur complement (bigIRT_laplace_blockdiag_solver_cpp).
//
// This used to be assembled in R: sparse V and W, a CHOLMOD factorisation of
// the N x N person block, and one Fisher-vector product per shared column.
// With eight threads on 10,000 items that single-threaded build was about 45
// per cent of wall time. Here each person contributes, independently,
//
//   item block j :  sum_{r in i,j} v_r v_r'  -  u_ij' Ipp_i^-1 u_ij,  u_ij = sum_{r in i,j} w_r v_r'
//   shared block :  sum_{r in i}   g_r g_r'  -  c_i'  Ipp_i^-1 c_i,   c_i  = sum_{r in i} w_r g_r' - P0 E_mu
//
// with v_r and g_r the row's entries in the item's and the shared columns.
// The R version assumed a person answers an item once, which reduces u to a
// single row; the sums above do not need that.
//
// Parameters are in the optimiser's coordinates: every derivative is scaled
// by its par_scale, as the R assembly did with V %*% S.

#include <RcppEigen.h>
#include <RcppParallel.h>
#include <vector>
#include <memory>
#include <cmath>
#include <algorithm>
#include "bigirt_obsindex.h"
using Eigen::MatrixXd;
using Eigen::VectorXd;

namespace {

inline double fb_inv_logit(const double x) {
  if (x >= 0.0) { const double z = std::exp(-x); return 1.0 / (1.0 + z); }
  const double z = std::exp(x); return z / (1.0 + z);
}
inline double fb_softplus(const double x) {
  return x > 0.0 ? x + std::log1p(std::exp(-x)) : std::log1p(std::exp(x));
}

// A read-only view of an R array, or an owned copy when R handed over a
// different storage type (a view of a coerced temporary would dangle). Views
// are taken on the main thread at the .Call boundary; the array outlives the
// call. See BigIRTOwnedVector in laplace_backend.cpp for why not copies.
template <typename T>
struct FbVec {
  std::shared_ptr< std::vector<T> > v;
  const T* p = nullptr;
  std::size_t len = 0;
  int nr = 0, nc = 0;
  FbVec() : v(std::make_shared< std::vector<T> >()) {}
  inline const T& operator[](std::size_t i) const { return p[i]; }
  inline const T& operator()(int i, int j) const {
    return p[static_cast<std::size_t>(i) + static_cast<std::size_t>(nr) * static_cast<std::size_t>(j)];
  }
  inline std::size_t count() const { return len; }
  inline int size() const { return static_cast<int>(len); }
};
inline FbVec<double> FbVec_d(SEXP x) {
  FbVec<double> o;
  o.len = static_cast<std::size_t>(Rf_xlength(x));
  o.nr = Rf_isMatrix(x) ? Rf_nrows(x) : static_cast<int>(o.len);
  o.nc = Rf_isMatrix(x) ? Rf_ncols(x) : 1;
  if (o.nr == 0) o.nc = 0;
  if (TYPEOF(x) == REALSXP) { if (o.len) o.p = REAL(x); }
  else { Rcpp::NumericVector r(x); o.v = std::make_shared< std::vector<double> >(r.begin(), r.end());
    o.p = o.v->data(); }
  return o;
}
inline FbVec<int> FbVec_i(SEXP x) {
  FbVec<int> o;
  o.len = static_cast<std::size_t>(Rf_xlength(x));
  o.nr = Rf_isMatrix(x) ? Rf_nrows(x) : static_cast<int>(o.len);
  o.nc = Rf_isMatrix(x) ? Rf_ncols(x) : 1;
  if (o.nr == 0) o.nc = 0;
  if (TYPEOF(x) == INTSXP) { if (o.len) o.p = INTEGER(x); }
  else if (TYPEOF(x) == LGLSXP) { if (o.len) o.p = LOGICAL(x); }
  else { Rcpp::IntegerVector r(x); o.v = std::make_shared< std::vector<int> >(r.begin(), r.end());
    o.p = o.v->data(); }
  return o;
}
inline FbVec<double> fb_d(SEXP x) { return FbVec_d(x); }
inline FbVec<int> fb_i(SEXP x) { return FbVec_i(x); }

// Everything a row contributes: its item, its derivative entries against
// free parameters (0-based global index, value in optimiser coordinates) and
// its person-ability derivatives w (K; zero where the ability is fixed).
struct FbRow { int item; std::vector<std::pair<int, double> > v; VectorXd w; };

struct FbData {
  FbVec<int> items, fixed_ability, A_ref, B_ref, C_ref, D_ref;
  // 1 where a free loading takes the identity link rather than softplus
  // (fitIRT's signedLoadings; see the note on the link in laplace_backend.cpp).
  FbVec<int> A_signed;
  FbVec<double> theta_mode, person_pred, fixed_ability_value, A_fixed_value, A_pred,
    B_fixed_value, B_pred, C_fixed_value, C_pred, D_fixed_value, D_pred,
    Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta, ps;
  FbVec<int> lay_A, lay_B, lay_C, lay_D, lay_Ab, lay_Bb, lay_Cb, lay_Db, lay_abil;
  int K = 1, P = 0;

  inline double beta_term(int o, const FbVec<double>& pred, const FbVec<double>& beta, int brow) const {
    if (brow <= 0 || pred.nc == 0 || beta.nr == 0) return 0.0;
    double s = 0.0;
    for (int j = 0; j < pred.nc; ++j) s += pred(o, j) * beta(brow - 1, j);
    return s;
  }
  inline void push(FbRow& row, const FbVec<int>& lay, int ix0, double val) const {
    if (ix0 < 0 || ix0 >= lay.size()) return;
    const int g = lay[ix0] - 1;
    if (g >= 0 && std::isfinite(val)) row.v.push_back(std::make_pair(g, val * ps[g]));
  }
  inline void push_beta(FbRow& row, const FbVec<int>& lay, const FbVec<double>& pred,
                        const FbVec<double>& beta, int o, int brow, double base) const {
    if (brow <= 0 || pred.nc == 0 || beta.nr == 0 || lay.size() == 0) return;
    for (int m = 0; m < pred.nc; ++m) push(row, lay, m * beta.nr + brow - 1, base * pred(o, m));
  }

  // The parameters row o touches, without their values: what the ownership
  // pass needs. It depends on the data and layout only, so it costs index
  // lookups where evaluating every row's derivatives, serially, did not.
  inline void touch_one(std::vector<int>& out, const FbVec<int>& lay, int ix0) const {
    if (ix0 < 0 || ix0 >= lay.size()) return;
    const int g = lay[ix0] - 1;
    if (g >= 0) out.push_back(g);
  }
  inline void touch_beta(std::vector<int>& out, const FbVec<int>& lay, const FbVec<double>& pred,
                         const FbVec<double>& beta, int brow) const {
    if (brow <= 0 || pred.nc == 0 || beta.nr == 0 || lay.size() == 0) return;
    for (int m = 0; m < pred.nc; ++m) touch_one(out, lay, m * beta.nr + brow - 1);
  }
  void touch(int o, std::vector<int>& out) const {
    out.clear();
    if (B_ref[o] > 0) { touch_one(out, lay_B, B_ref[o] - 1);
      touch_beta(out, lay_Bb, B_pred, Bbeta, Bbeta.nr == 1 ? 1 : B_ref[o]); }
    for (int k = 0; k < K; ++k) if (A_ref(o, k) > 0) {
      touch_one(out, lay_A, A_ref(o, k) - 1);
      touch_beta(out, lay_Ab, A_pred, invspAbeta, invspAbeta.nr == 1 ? 1 : A_ref(o, k));
    }
    if (C_ref[o] > 0) { touch_one(out, lay_C, C_ref[o] - 1);
      touch_beta(out, lay_Cb, C_pred, logitCbeta, logitCbeta.nr == 1 ? 1 : C_ref[o]); }
    if (D_ref[o] > 0) { touch_one(out, lay_D, D_ref[o] - 1);
      touch_beta(out, lay_Db, D_pred, logitDbeta, logitDbeta.nr == 1 ? 1 : D_ref[o]); }
    for (int k = 0; k < K; ++k) if (fixed_ability(o, k) == 0)
      for (int m = 0; m < P; ++m) touch_one(out, lay_abil, k + K * m);
  }

  FbRow row(int o, int subj) const {
    FbRow r; r.item = items[o] - 1; r.w = VectorXd::Zero(K);
    const int bb = B_ref[o] > 0 ? (Bbeta.nr == 1 ? 1 : B_ref[o]) : 0;
    const double b = B_ref[o] > 0 ? Bpars[B_ref[o] - 1] + beta_term(o, B_pred, Bbeta, bb) : B_fixed_value[o];
    double c = C_fixed_value[o], cm = 0.0;
    const int cb = C_ref[o] > 0 ? (logitCbeta.nr == 1 ? 1 : C_ref[o]) : 0;
    if (C_ref[o] > 0) { const double sg = fb_inv_logit(logitCpars[C_ref[o] - 1] + beta_term(o, C_pred, logitCbeta, cb));
      c = 0.5 * sg; cm = 0.5 * sg * (1.0 - sg); }
    double d = D_fixed_value[o], dm = 0.0;
    const int db = D_ref[o] > 0 ? (logitDbeta.nr == 1 ? 1 : D_ref[o]) : 0;
    if (D_ref[o] > 0) { const double sg = fb_inv_logit(logitDpars[D_ref[o] - 1] + beta_term(o, D_pred, logitDbeta, db));
      d = 0.5 * sg + 0.5; dm = 0.5 * sg * (1.0 - sg); }
    VectorXd a(K), am(K), ab(K);
    double eta = -b;
    for (int k = 0; k < K; ++k) {
      a(k) = A_fixed_value(o, k); am(k) = 0.0;
      if (A_ref(o, k) > 0) {
        const int abx = invspAbeta.nr == 1 ? 1 : A_ref(o, k);
        const double raw = invspApars[A_ref(o, k) - 1] + beta_term(o, A_pred, invspAbeta, abx);
        if (A_signed[A_ref(o, k) - 1] != 0) { a(k) = raw; am(k) = 1.0; }   // identity link
        else { a(k) = fb_softplus(raw); am(k) = fb_inv_logit(raw); }
      }
      double of = 0.0;
      if (fixed_ability(o, k) == 0) for (int j = 0; j < P; ++j) of += person_pred(o, j) * Abilitybeta(k, j);
      ab(k) = fixed_ability(o, k) != 0 ? fixed_ability_value(o, k) : theta_mode(subj, k) + of;
      eta += a(k) * ab(k);
    }
    const double g = fb_inv_logit(eta);
    double p = c + (d - c) * g;
    p = std::min(std::max(p, 1e-12), 1.0 - 1e-12);
    const double s = (d - c) * g * (1.0 - g);
    const double wt = 1.0 / std::sqrt(std::max(p * (1.0 - p), 1e-12));
    if (B_ref[o] > 0) { push(r, lay_B, B_ref[o] - 1, -s * wt); push_beta(r, lay_Bb, B_pred, Bbeta, o, bb, -s * wt); }
    for (int k = 0; k < K; ++k) if (A_ref(o, k) > 0) {
      const double vA = s * am(k) * ab(k) * wt;
      push(r, lay_A, A_ref(o, k) - 1, vA);
      push_beta(r, lay_Ab, A_pred, invspAbeta, o, invspAbeta.nr == 1 ? 1 : A_ref(o, k), vA);
    }
    if (C_ref[o] > 0) { const double vC = (1.0 - g) * cm * wt;
      push(r, lay_C, C_ref[o] - 1, vC); push_beta(r, lay_Cb, C_pred, logitCbeta, o, cb, vC); }
    if (D_ref[o] > 0) { const double vD = g * dm * wt;
      push(r, lay_D, D_ref[o] - 1, vD); push_beta(r, lay_Db, D_pred, logitDbeta, o, db, vD); }
    for (int k = 0; k < K; ++k) if (fixed_ability(o, k) == 0) {
      r.w(k) = s * a(k) * wt;
      for (int m = 0; m < P; ++m) push(r, lay_abil, k + K * m, s * a(k) * person_pred(o, m) * wt);
    }
    return r;
  }
};

struct FbWorker : public RcppParallel::Worker {
  const FbData& D;
  const BigIRTObsIndex& obs_by_subj;
  const std::vector<int>& owner;   // block of each parameter, -1 in G
  const std::vector<int>& bpos;    // position within its block (or within G)
  const std::vector<int>& bdim;    // size of each block
  const std::vector<long>& bofs;   // offset of each block in `blocks`
  const MatrixXd& P0;
  const std::vector<int>& amean;   // 0-based global indices of the ability mean, K or empty
  const int nG;
  std::vector<double> blocks;
  MatrixXd FGG;
  int failure = 0;

  FbWorker(const FbData& D, const BigIRTObsIndex& obs_by_subj,
           const std::vector<int>& owner, const std::vector<int>& bpos, const std::vector<int>& bdim,
           const std::vector<long>& bofs, long total, const MatrixXd& P0,
           const std::vector<int>& amean, int nG)
    : D(D), obs_by_subj(obs_by_subj), owner(owner), bpos(bpos), bdim(bdim), bofs(bofs),
      P0(P0), amean(amean), nG(nG), blocks(static_cast<std::size_t>(total), 0.0),
      FGG(MatrixXd::Zero(nG, nG)) {}
  FbWorker(const FbWorker& o, RcppParallel::Split)
    : D(o.D), obs_by_subj(o.obs_by_subj), owner(o.owner), bpos(o.bpos), bdim(o.bdim), bofs(o.bofs),
      P0(o.P0), amean(o.amean), nG(o.nG), blocks(o.blocks.size(), 0.0), FGG(MatrixXd::Zero(o.nG, o.nG)) {}

  void operator()(std::size_t begin, std::size_t end) {
    const int K = D.K;
    for (std::size_t s = begin; s < end; ++s) {
      const int subj = static_cast<int>(s);
      const BigIRTObsSpan obs = obs_by_subj[subj];
      std::vector<FbRow> rows; rows.reserve(obs.size());
      MatrixXd Ipp = P0;
      for (int o : obs) { rows.push_back(D.row(o, subj)); Ipp += rows.back().w * rows.back().w.transpose(); }
      Eigen::LLT<MatrixXd> llt(Ipp);
      if (llt.info() != Eigen::Success) { failure = 1; continue; }
      // u for each (person, item) block, and c for the shared block.
      std::vector<int> u_item; std::vector<MatrixXd> u_mat;
      MatrixXd c = MatrixXd::Zero(K, nG);
      if (!amean.empty()) for (int k = 0; k < K; ++k) {
        const int gp = bpos[amean[k]];
        for (int kk = 0; kk < K; ++kk) c(kk, gp) -= P0(kk, k) * D.ps[amean[k]];
      }
      for (const FbRow& r : rows) {
        for (std::size_t x = 0; x < r.v.size(); ++x) {
          const int px = r.v[x].first; const double vx = r.v[x].second;
          const int bx = owner[px];
          if (bx < 0) {
            const int gx = bpos[px];
            c.col(gx) += r.w * vx;
            for (std::size_t y = 0; y < r.v.size(); ++y) {
              const int py = r.v[y].first;
              if (owner[py] < 0) FGG(gx, bpos[py]) += vx * r.v[y].second;
            }
          } else {
            const long base = bofs[bx]; const int dim = bdim[bx];
            for (std::size_t y = 0; y < r.v.size(); ++y) {
              const int py = r.v[y].first;
              if (owner[py] == bx) blocks[static_cast<std::size_t>(base + bpos[px] + dim * bpos[py])] += vx * r.v[y].second;
            }
            std::size_t ui = 0;
            while (ui < u_item.size() && u_item[ui] != bx) ++ui;
            if (ui == u_item.size()) { u_item.push_back(bx); u_mat.push_back(MatrixXd::Zero(K, dim)); }
            u_mat[ui].col(bpos[px]) += r.w * vx;
          }
        }
      }
      for (std::size_t ui = 0; ui < u_item.size(); ++ui) {
        const int bx = u_item[ui]; const int dim = bdim[bx]; const long base = bofs[bx];
        const MatrixXd sub = u_mat[ui].transpose() * llt.solve(u_mat[ui]);
        for (int j = 0; j < dim; ++j) for (int i = 0; i < dim; ++i)
          blocks[static_cast<std::size_t>(base + i + dim * j)] -= sub(i, j);
      }
      if (nG > 0) FGG -= c.transpose() * llt.solve(c);
    }
  }
  void join(const FbWorker& o) {
    for (std::size_t i = 0; i < blocks.size(); ++i) blocks[i] += o.blocks[i];
    FGG += o.FGG;
    if (o.failure) failure = o.failure;
  }
};

// The full Fisher information, dense, for fits small enough to hold it:
//   F = sum_i [ sum_{r in i} v_r v_r'  -  c_i' Ipp_i^-1 c_i ],  c_i = sum_{r in i} w_r v_r' - P0 E_mu,
// over the parameters person i touches, so the coupling between items that
// runs through shared persons is all there. The Schur term is a rank-K
// update, B'B with B = L^-1 c_i. Replaces an R assembly (sparse V and W,
// CHOLMOD on the person block) that was two thirds of a nine-dimensional
// fit's wall time.
struct FbDenseWorker : public RcppParallel::Worker {
  const FbData& D;
  const BigIRTObsIndex& obs_by_subj;
  const MatrixXd& P0;
  const std::vector<int>& amean;
  const int n;
  MatrixXd F;
  int failure = 0;
  FbDenseWorker(const FbData& D, const BigIRTObsIndex& obs, const MatrixXd& P0,
                const std::vector<int>& amean, int n)
    : D(D), obs_by_subj(obs), P0(P0), amean(amean), n(n), F(MatrixXd::Zero(n, n)) {}
  FbDenseWorker(const FbDenseWorker& o, RcppParallel::Split)
    : D(o.D), obs_by_subj(o.obs_by_subj), P0(o.P0), amean(o.amean), n(o.n), F(MatrixXd::Zero(o.n, o.n)) {}
  void operator()(std::size_t begin, std::size_t end) {
    const int K = D.K;
    std::vector<int> loc(static_cast<std::size_t>(n), -1);
    for (std::size_t s = begin; s < end; ++s) {
      const int subj = static_cast<int>(s);
      const BigIRTObsSpan obs = obs_by_subj[subj];
      std::vector<FbRow> rows; rows.reserve(obs.size());
      MatrixXd Ipp = P0;
      for (int o : obs) { rows.push_back(D.row(o, subj)); Ipp += rows.back().w * rows.back().w.transpose(); }
      Eigen::LLT<MatrixXd> llt(Ipp);
      if (llt.info() != Eigen::Success) { failure = 1; continue; }
      std::vector<int> touched;
      auto slot = [&](int p) { if (loc[p] < 0) { loc[p] = static_cast<int>(touched.size()); touched.push_back(p); } return loc[p]; };
      for (const FbRow& r : rows) for (const auto& e : r.v) slot(e.first);
      for (int p : amean) slot(p);
      const int m = static_cast<int>(touched.size());
      MatrixXd c = MatrixXd::Zero(K, m);
      for (std::size_t k = 0; k < amean.size(); ++k)
        for (int kk = 0; kk < K; ++kk) c(kk, loc[amean[k]]) -= P0(kk, static_cast<int>(k)) * D.ps[amean[k]];
      for (const FbRow& r : rows) {
        for (const auto& ex : r.v) {
          c.col(loc[ex.first]) += r.w * ex.second;
          for (const auto& ey : r.v) F(ex.first, ey.first) += ex.second * ey.second;
        }
      }
      // Only over the columns where the coupling is nonzero: the term is
      // B'B, and a zero column of c gives a zero row and column of it. A
      // person whose every ability is fixed has c = 0 (unless the ability mean
      // is estimated), so the whole m x m product was zeros, formed and
      // scattered anyway -- at 500 items a person, 2,000 such persons cost 26 s
      // a metric build, against 2 s for the whole of the optimisation.
      std::vector<int> nz;
      nz.reserve(static_cast<std::size_t>(m));
      for (int j = 0; j < m; ++j) if (c.col(j).squaredNorm() > 0.0) nz.push_back(j);
      if (!nz.empty()) {
        MatrixXd cz(K, static_cast<int>(nz.size()));
        for (std::size_t j = 0; j < nz.size(); ++j) cz.col(static_cast<int>(j)) = c.col(nz[j]);
        const MatrixXd B = llt.matrixL().solve(cz);
        const MatrixXd BB = B.transpose() * B;
        for (std::size_t j = 0; j < nz.size(); ++j) for (std::size_t i = 0; i < nz.size(); ++i)
          F(touched[nz[i]], touched[nz[j]]) -= BB(static_cast<int>(i), static_cast<int>(j));
      }
      for (int p : touched) loc[p] = -1;
    }
  }
  void join(const FbDenseWorker& o) { F += o.F; if (o.failure) failure = o.failure; }
};

} // namespace

// Returns the inverse of every item block (with its prior curvature added)
// and the shared block without priors; R adds the shared priors, any
// finite-difference columns, and factors that small dense block.
static SEXP fb_impl(const bool dense,
    SEXP idSEXP, SEXP itemsSEXP, SEXP theta_modeSEXP, SEXP person_predSEXP,
    SEXP fixed_abilitySEXP, SEXP fixed_ability_valueSEXP,
    SEXP A_refSEXP, SEXP A_fixed_valueSEXP, SEXP A_predSEXP, SEXP A_signedSEXP,
    SEXP B_refSEXP, SEXP B_fixed_valueSEXP, SEXP B_predSEXP,
    SEXP C_refSEXP, SEXP C_fixed_valueSEXP, SEXP C_predSEXP,
    SEXP D_refSEXP, SEXP D_fixed_valueSEXP, SEXP D_predSEXP,
    SEXP AbilitybetaSEXP, SEXP invspAparsSEXP, SEXP invspAbetaSEXP, SEXP BparsSEXP, SEXP BbetaSEXP,
    SEXP logitCparsSEXP, SEXP logitCbetaSEXP, SEXP logitDparsSEXP, SEXP logitDbetaSEXP,
    SEXP P0SEXP, SEXP lay_ASEXP, SEXP lay_BSEXP, SEXP lay_CSEXP, SEXP lay_DSEXP,
    SEXP lay_AbSEXP, SEXP lay_BbSEXP, SEXP lay_CbSEXP, SEXP lay_DbSEXP,
    SEXP lay_abilSEXP, SEXP lay_ameanSEXP, SEXP item_parSEXP, SEXP psSEXP, SEXP prior_diagSEXP,
    SEXP grainSEXP) {
  Rcpp::IntegerVector id(idSEXP);
  Rcpp::NumericMatrix theta_mode(theta_modeSEXP);
  Rcpp::IntegerVector item_par(item_parSEXP);
  Rcpp::NumericVector prior_diag(prior_diagSEXP);
  Rcpp::NumericMatrix P0r(P0SEXP);
  const int Nobs = id.size(), N = theta_mode.nrow(), K = theta_mode.ncol();
  const int n = item_par.size();
  FbData D;
  D.items = fb_i(itemsSEXP); D.fixed_ability = fb_i(fixed_abilitySEXP);
  D.A_ref = fb_i(A_refSEXP); D.B_ref = fb_i(B_refSEXP); D.C_ref = fb_i(C_refSEXP); D.D_ref = fb_i(D_refSEXP);
  D.theta_mode = fb_d(theta_modeSEXP); D.person_pred = fb_d(person_predSEXP);
  D.fixed_ability_value = fb_d(fixed_ability_valueSEXP);
  D.A_fixed_value = fb_d(A_fixed_valueSEXP); D.A_pred = fb_d(A_predSEXP);
  D.B_fixed_value = fb_d(B_fixed_valueSEXP); D.B_pred = fb_d(B_predSEXP);
  D.C_fixed_value = fb_d(C_fixed_valueSEXP); D.C_pred = fb_d(C_predSEXP);
  D.D_fixed_value = fb_d(D_fixed_valueSEXP); D.D_pred = fb_d(D_predSEXP);
  D.Abilitybeta = fb_d(AbilitybetaSEXP); D.invspApars = fb_d(invspAparsSEXP); D.invspAbeta = fb_d(invspAbetaSEXP);
  D.Bpars = fb_d(BparsSEXP); D.Bbeta = fb_d(BbetaSEXP); D.logitCpars = fb_d(logitCparsSEXP);
  D.logitCbeta = fb_d(logitCbetaSEXP); D.logitDpars = fb_d(logitDparsSEXP); D.logitDbeta = fb_d(logitDbetaSEXP);
  D.ps = fb_d(psSEXP);
  D.A_signed = fb_i(A_signedSEXP);
  if (D.A_signed.count() != D.invspApars.count())
    Rcpp::stop("Fisher information: A_signed must hold one entry per free loading.");
  D.lay_A = fb_i(lay_ASEXP); D.lay_B = fb_i(lay_BSEXP); D.lay_C = fb_i(lay_CSEXP); D.lay_D = fb_i(lay_DSEXP);
  D.lay_Ab = fb_i(lay_AbSEXP); D.lay_Bb = fb_i(lay_BbSEXP); D.lay_Cb = fb_i(lay_CbSEXP); D.lay_Db = fb_i(lay_DbSEXP);
  D.lay_abil = fb_i(lay_abilSEXP);
  D.K = K;
  D.P = (D.person_pred.nc > 0 && D.Abilitybeta.nc == D.person_pred.nc) ? D.person_pred.nc : 0;
  Rcpp::IntegerVector lay_amean(lay_ameanSEXP);
  std::vector<int> amean;
  for (int k = 0; k < lay_amean.size(); ++k) amean.push_back(lay_amean[k] - 1);

  BigIRTObsIndex obs_by_subj;
  if (!obs_by_subj.build(id, Nobs, N)) Rcpp::stop("id must be coded from 1 to Nsubs.");

  if (dense) {
    MatrixXd P0d(K, K);
    for (int r = 0; r < K; ++r) for (int c = 0; c < K; ++c) P0d(r, c) = P0r(r, c);
    FbDenseWorker wd(D, obs_by_subj, P0d, amean, n);
    RcppParallel::parallelReduce(static_cast<std::size_t>(0), static_cast<std::size_t>(N), wd,
                                 static_cast<std::size_t>(std::max(1, Rcpp::as<int>(grainSEXP))));
    if (wd.failure) Rcpp::stop("Fisher information: a person information block could not be factored.");
    Rcpp::NumericMatrix out(n, n);
    for (int j = 0; j < n; ++j) for (int i = 0; i < n; ++i) out(i, j) = 0.5 * (wd.F(i, j) + wd.F(j, i));
    return out;
  }

  // Which parameters are local to one item: every row that touches them
  // belongs to that item. Serial, one pass; the row entries are rebuilt in
  // the parallel pass rather than stored.
  const int UNSET = -2, SHARED = -1;
  std::vector<int> own_item(static_cast<std::size_t>(n), UNSET);
  {
    std::vector<int> tp;
    for (int o = 0; o < Nobs; ++o) {
      D.touch(o, tp);
      const int item = D.items[o] - 1;
      for (int g : tp) {
        int& ow = own_item[static_cast<std::size_t>(g)];
        if (ow == UNSET) ow = item; else if (ow != item) ow = SHARED;
      }
    }
  }
  // Blocks: one per item for its local parameters; an item parameter no row
  // reaches gets a block of its own. Everything else is shared (G), the
  // ability mean always among it.
  std::vector<int> owner(static_cast<std::size_t>(n), -1), bpos(static_cast<std::size_t>(n), -1), bdim;
  std::vector<long> bofs;
  std::vector<int> item_block;
  int nG = 0; long total = 0;
  {
    int maxitem = 0;
    for (int o = 0; o < Nobs; ++o) maxitem = std::max(maxitem, D.items[o]);
    item_block.assign(static_cast<std::size_t>(maxitem), -1);
  }
  std::vector<int> Gidx;
  for (int p = 0; p < n; ++p) {
    const int ow = own_item[p];
    const bool is_amean = std::find(amean.begin(), amean.end(), p) != amean.end();
    if (!is_amean && ow >= 0) {
      int& b = item_block[static_cast<std::size_t>(ow)];
      if (b < 0) { b = static_cast<int>(bdim.size()); bdim.push_back(0); }
      owner[p] = b; bpos[p] = bdim[b]++;
    } else if (!is_amean && ow == UNSET && item_par[p] != 0) {
      owner[p] = static_cast<int>(bdim.size()); bpos[p] = 0; bdim.push_back(1);
    } else {
      owner[p] = -1; bpos[p] = nG++; Gidx.push_back(p + 1);
    }
  }
  bofs.resize(bdim.size());
  for (std::size_t b = 0; b < bdim.size(); ++b) { bofs[b] = total; total += static_cast<long>(bdim[b]) * bdim[b]; }

  MatrixXd P0(K, K);
  for (int r = 0; r < K; ++r) for (int c = 0; c < K; ++c) P0(r, c) = P0r(r, c);
  FbWorker w(D, obs_by_subj, owner, bpos, bdim, bofs, total, P0, amean, nG);
  RcppParallel::parallelReduce(static_cast<std::size_t>(0), static_cast<std::size_t>(N), w,
                               static_cast<std::size_t>(std::max(1, Rcpp::as<int>(grainSEXP))));
  if (w.failure) Rcpp::stop("Fisher blocks: a person information block could not be factored.");

  // Parameters of each block, in block order, and the inverse of each block
  // with its (diagonal) prior curvature added. A block with no curvature at
  // all -- no data, no prior -- gets a median diagonal, as the R assembly did.
  std::vector< std::vector<int> > bpar(bdim.size());
  for (int p = 0; p < n; ++p) if (owner[p] >= 0) {
    std::vector<int>& v = bpar[static_cast<std::size_t>(owner[p])];
    if (static_cast<int>(v.size()) <= bpos[p]) v.resize(static_cast<std::size_t>(bpos[p]) + 1, -1);
    v[static_cast<std::size_t>(bpos[p])] = p;
  }
  // A diagonal not above 1e-12 of the largest is numerically flat -- a loading
  // on the flat tail of its softplus, curvature 1e-108 -- and is decoupled and
  // given the median below, as bigIRT_metric_decouple does in R: left in, its
  // inverse moved that coordinate by 1e53 and no line search could recover.
  double top = 0;
  for (std::size_t b = 0; b < bdim.size(); ++b) for (int i = 0; i < bdim[b]; ++i) {
    const double x = w.blocks[static_cast<std::size_t>(bofs[b] + i + bdim[b] * i)] + prior_diag[bpar[b][i]];
    if (x > top) top = x;
  }
  const double flat = 1e-12 * top;
  std::vector<double> dg;
  for (std::size_t b = 0; b < bdim.size(); ++b) for (int i = 0; i < bdim[b]; ++i) {
    const double x = w.blocks[static_cast<std::size_t>(bofs[b] + i + bdim[b] * i)] + prior_diag[bpar[b][i]];
    if (x > flat) dg.push_back(x);
  }
  double med = 1e-8;
  if (!dg.empty()) { std::nth_element(dg.begin(), dg.begin() + dg.size() / 2, dg.end()); med = std::max(dg[dg.size() / 2], 1e-8); }
  Rcpp::NumericVector inv(static_cast<R_xlen_t>(total));
  Rcpp::IntegerVector block_par, block_dim(static_cast<R_xlen_t>(bdim.size()));
  std::vector<int> bp_flat;
  int ridged = 0;
  for (std::size_t b = 0; b < bdim.size(); ++b) {
    const int dim = bdim[b];
    MatrixXd M(dim, dim);
    for (int j = 0; j < dim; ++j) for (int i = 0; i < dim; ++i)
      M(i, j) = w.blocks[static_cast<std::size_t>(bofs[b] + i + dim * j)];
    for (int i = 0; i < dim; ++i) { M(i, i) += prior_diag[bpar[b][i]]; if (!(M(i, i) > 0)) M(i, i) = med; }
    for (int i = 0; i < dim; ++i) if (M(i, i) > 0 && !(M(i, i) > flat)) {
      M.row(i).setZero(); M.col(i).setZero(); M(i, i) = med; }
    M = 0.5 * (M + M.transpose());
    Eigen::LLT<MatrixXd> llt(M);
    double ridge = 1e-8 * M.diagonal().cwiseAbs().maxCoeff();
    for (int a = 0; a < 6 && llt.info() != Eigen::Success; ++a) {
      llt.compute(M + ridge * MatrixXd::Identity(dim, dim)); ridge *= 100; ++ridged; }
    const MatrixXd Mi = llt.solve(MatrixXd::Identity(dim, dim));
    for (int j = 0; j < dim; ++j) for (int i = 0; i < dim; ++i) inv[bofs[b] + i + dim * j] = Mi(i, j);
    block_dim[b] = dim;
    for (int i = 0; i < dim; ++i) bp_flat.push_back(bpar[b][i] + 1);
  }
  block_par = Rcpp::IntegerVector(bp_flat.begin(), bp_flat.end());
  Rcpp::NumericMatrix FGG(nG, nG);
  for (int j = 0; j < nG; ++j) for (int i = 0; i < nG; ++i) FGG(i, j) = 0.5 * (w.FGG(i, j) + w.FGG(j, i));
  return Rcpp::List::create(
    Rcpp::Named("block_inv") = inv, Rcpp::Named("block_dim") = block_dim,
    Rcpp::Named("block_par") = block_par, Rcpp::Named("G") = Rcpp::IntegerVector(Gidx.begin(), Gidx.end()),
    Rcpp::Named("FGG") = FGG, Rcpp::Named("ridged") = ridged);
}

extern "C" SEXP _bigIRT_fisher_blocks_cpp_impl(
    SEXP idSEXP, SEXP itemsSEXP, SEXP theta_modeSEXP, SEXP person_predSEXP,
    SEXP fixed_abilitySEXP, SEXP fixed_ability_valueSEXP,
    SEXP A_refSEXP, SEXP A_fixed_valueSEXP, SEXP A_predSEXP, SEXP A_signedSEXP,
    SEXP B_refSEXP, SEXP B_fixed_valueSEXP, SEXP B_predSEXP,
    SEXP C_refSEXP, SEXP C_fixed_valueSEXP, SEXP C_predSEXP,
    SEXP D_refSEXP, SEXP D_fixed_valueSEXP, SEXP D_predSEXP,
    SEXP AbilitybetaSEXP, SEXP invspAparsSEXP, SEXP invspAbetaSEXP, SEXP BparsSEXP, SEXP BbetaSEXP,
    SEXP logitCparsSEXP, SEXP logitCbetaSEXP, SEXP logitDparsSEXP, SEXP logitDbetaSEXP,
    SEXP P0SEXP, SEXP lay_ASEXP, SEXP lay_BSEXP, SEXP lay_CSEXP, SEXP lay_DSEXP,
    SEXP lay_AbSEXP, SEXP lay_BbSEXP, SEXP lay_CbSEXP, SEXP lay_DbSEXP,
    SEXP lay_abilSEXP, SEXP lay_ameanSEXP, SEXP item_parSEXP, SEXP psSEXP, SEXP prior_diagSEXP,
    SEXP grainSEXP) {
  BEGIN_RCPP
  return fb_impl(false, idSEXP, itemsSEXP, theta_modeSEXP, person_predSEXP, fixed_abilitySEXP, fixed_ability_valueSEXP, A_refSEXP, A_fixed_valueSEXP, A_predSEXP, A_signedSEXP, B_refSEXP, B_fixed_valueSEXP, B_predSEXP, C_refSEXP, C_fixed_valueSEXP, C_predSEXP, D_refSEXP, D_fixed_valueSEXP, D_predSEXP, AbilitybetaSEXP, invspAparsSEXP, invspAbetaSEXP, BparsSEXP, BbetaSEXP, logitCparsSEXP, logitCbetaSEXP, logitDparsSEXP, logitDbetaSEXP, P0SEXP, lay_ASEXP, lay_BSEXP, lay_CSEXP, lay_DSEXP, lay_AbSEXP, lay_BbSEXP, lay_CbSEXP, lay_DbSEXP, lay_abilSEXP, lay_ameanSEXP, item_parSEXP, psSEXP, prior_diagSEXP, grainSEXP);
  END_RCPP
}

// The dense Fisher information (without priors), same arguments; item_par,
// prior_diag are unused.
extern "C" SEXP _bigIRT_fisher_dense_cpp_impl(
    SEXP idSEXP, SEXP itemsSEXP, SEXP theta_modeSEXP, SEXP person_predSEXP,
    SEXP fixed_abilitySEXP, SEXP fixed_ability_valueSEXP,
    SEXP A_refSEXP, SEXP A_fixed_valueSEXP, SEXP A_predSEXP, SEXP A_signedSEXP,
    SEXP B_refSEXP, SEXP B_fixed_valueSEXP, SEXP B_predSEXP,
    SEXP C_refSEXP, SEXP C_fixed_valueSEXP, SEXP C_predSEXP,
    SEXP D_refSEXP, SEXP D_fixed_valueSEXP, SEXP D_predSEXP,
    SEXP AbilitybetaSEXP, SEXP invspAparsSEXP, SEXP invspAbetaSEXP, SEXP BparsSEXP, SEXP BbetaSEXP,
    SEXP logitCparsSEXP, SEXP logitCbetaSEXP, SEXP logitDparsSEXP, SEXP logitDbetaSEXP,
    SEXP P0SEXP, SEXP lay_ASEXP, SEXP lay_BSEXP, SEXP lay_CSEXP, SEXP lay_DSEXP,
    SEXP lay_AbSEXP, SEXP lay_BbSEXP, SEXP lay_CbSEXP, SEXP lay_DbSEXP,
    SEXP lay_abilSEXP, SEXP lay_ameanSEXP, SEXP item_parSEXP, SEXP psSEXP, SEXP prior_diagSEXP,
    SEXP grainSEXP) {
  BEGIN_RCPP
  return fb_impl(true, idSEXP, itemsSEXP, theta_modeSEXP, person_predSEXP, fixed_abilitySEXP, fixed_ability_valueSEXP, A_refSEXP, A_fixed_valueSEXP, A_predSEXP, A_signedSEXP, B_refSEXP, B_fixed_valueSEXP, B_predSEXP, C_refSEXP, C_fixed_valueSEXP, C_predSEXP, D_refSEXP, D_fixed_valueSEXP, D_predSEXP, AbilitybetaSEXP, invspAparsSEXP, invspAbetaSEXP, BparsSEXP, BbetaSEXP, logitCparsSEXP, logitCbetaSEXP, logitDparsSEXP, logitDbetaSEXP, P0SEXP, lay_ASEXP, lay_BSEXP, lay_CSEXP, lay_DSEXP, lay_AbSEXP, lay_BbSEXP, lay_CbSEXP, lay_DbSEXP, lay_abilSEXP, lay_ameanSEXP, item_parSEXP, psSEXP, prior_diagSEXP, grainSEXP);
  END_RCPP
}
