// Second-order Laplace: the leading correction to each person's Laplace
// value, with its exact gradient.
//
// WHY. Beyond three dimensions a product quadrature rule is out of reach and
// sampled rules are noisy and, held fixed, exploitable (a 256-point rule at
// nine dimensions ran away to correlations 100 SE off). The second-order
// Laplace correction is deterministic, smooth, and costs O(n^2) per person
// whatever the dimension. Measured at nine dimensions with cross loadings
// (dev/optim/mdim_rules.R), per-person error in the log marginal against a
// 65,536-point reference: Laplace 0.095 (2PL) and 0.096 (3PL), corrected
// 0.0038 and 0.0032, against 0.012 for 256 quasi-Monte Carlo points.
//
// WHAT. With u_r = a_r'x for x ~ N(0, S), S = H^-1 the inverse observed
// Hessian at the mode, the log integrand's third- and fourth-order terms are
// T3 = sum_r l3_r u_r^3 / 6 and T4 = sum_r l4_r u_r^4 / 24, and
//
//   log E exp(T3 + T4) ~ E T4 + E T3^2 / 2
//     = 1/8 sum_r l4_r c_rr^2
//       + 1/24 sum_rs l3_r l3_s (3 c_rr c_ss c_rs + 2 c_rs^3),   c_rs = a_r' S a_s,
//
// l_k the k-th derivative of a response's log-likelihood in its linear
// predictor. That is C_i, added to person i's Laplace value.
//
// GRADIENT, per person by reverse accumulation. C depends on the rows'
// (eta, c, d, a) directly, on S through H = P0 + sum_r w_r a_r a_r'
// (w = -l2), and on the mode through eta. With Gbar = dC/dc_rs,
//   dC/dS = A' Gbar A,  dC/dH = Hbar = -S (A' Gbar A) S,
//   dC/dw_r = a_r' Hbar a_r,  dC/dP0 = Hbar,
// and the mode enters by the implicit function theorem through
// u = S dC/dm: each row's eta-gradient gains l2_r a_r'u, its asymptote
// gradients dl1/dc a_r'u, its loadings l1_r u, the ability mean P0 u, and the
// prior precision -u (m - mu)'. Where a person's data curvature is not
// positive semidefinite it is clipped to the nearest matrix that is (and
// counted), with H then held fixed in the rows, as the Laplace path does.
//
// CAUTION. With free asymptotes the correction is not bounded: where the
// observed curvature is not positive, S does not shrink as loadings grow and
// the E T3^2 term grows like a^6. A two-dimensional 3PL refinement exploited
// that (objective 1e13); fitIRT's "auto" uses this only without asymptotes,
// where the curvature is always positive, and guards every refinement.

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

// Minimal forward-mode dual number, for derivatives in one asymptote.
struct Dual {
  double v, d;
  Dual(double v_ = 0, double d_ = 0) : v(v_), d(d_) {}
};
inline Dual operator+(Dual a, Dual b) { return Dual(a.v + b.v, a.d + b.d); }
inline Dual operator-(Dual a, Dual b) { return Dual(a.v - b.v, a.d - b.d); }
inline Dual operator*(Dual a, Dual b) { return Dual(a.v * b.v, a.d * b.v + a.v * b.d); }
inline Dual operator/(Dual a, Dual b) { return Dual(a.v / b.v, (a.d * b.v - a.v * b.d) / (b.v * b.v)); }
inline Dual operator*(double s, Dual a) { return Dual(s * a.v, s * a.d); }
inline Dual operator-(double s, Dual a) { return Dual(s - a.v, -a.d); }
inline double val(double x) { return x; }
inline double val(const Dual& x) { return x.v; }

inline double l2_sig(const double x) {
  if (x >= 0.0) { const double z = std::exp(-x); return 1.0 / (1.0 + z); }
  const double z = std::exp(x); return z / (1.0 + z);
}

// Derivatives 1..5 of a response's log-likelihood in eta, for the 3PL/4PL
// link p = c + (d - c) sigma(eta), as cumulants of f = p (y = 1) or 1 - p.
// T is double or Dual (carrying a derivative in c or d).
template <typename T>
void l2_tower(const double y, const double eta, const T& c, const T& d, T out[6]) {
  const double s = l2_sig(eta);
  const double s1 = s * (1 - s);
  const double sd[6] = { s, s1, s1 * (1 - 2 * s), s1 * (1 - 6 * s + 6 * s * s),
    s1 * (1 - 2 * s) * (1 - 12 * s + 12 * s * s),
    s1 * (1 - 30 * s + 150 * s * s - 240 * s * s * s + 120 * s * s * s * s) };
  const T u = d - c;
  T p = c + s * u;
  if (val(p) < 1e-12) p = T(1e-12);
  if (val(p) > 1 - 1e-12) p = T(1 - 1e-12);
  const double sg = y > 0.5 ? 1.0 : -1.0;
  const T f0 = y > 0.5 ? p : (1.0 - p);
  T r[6];
  for (int j = 1; j <= 5; ++j) r[j] = (sg * sd[j]) * u / f0;
  out[0] = f0;
  out[1] = r[1];
  out[2] = r[2] - r[1] * r[1];
  out[3] = r[3] - 3.0 * r[1] * r[2] + 2.0 * r[1] * r[1] * r[1];
  out[4] = r[4] - 4.0 * r[1] * r[3] - 3.0 * r[2] * r[2] + 12.0 * r[1] * r[1] * r[2]
    - 6.0 * r[1] * r[1] * r[1] * r[1];
  out[5] = r[5] - 5.0 * r[1] * r[4] - 10.0 * r[2] * r[3] + 20.0 * r[1] * r[1] * r[3]
    + 30.0 * r[1] * r[2] * r[2] - 60.0 * r[1] * r[1] * r[1] * r[2]
    + 24.0 * r[1] * r[1] * r[1] * r[1] * r[1];
}

// A read-only view of an R array, or an owned copy when R handed over a
// different storage type (a view of a coerced temporary would dangle). Views
// are taken on the main thread at the .Call boundary; the array outlives the
// call. See BigIRTOwnedVector in laplace_backend.cpp for why not copies.
template <typename T>
struct L2Vec {
  std::shared_ptr< std::vector<T> > v;
  const T* p = nullptr;
  std::size_t len = 0;
  int nr = 0, nc = 0;
  L2Vec() : v(std::make_shared< std::vector<T> >()) {}
  inline const T& operator[](std::size_t i) const { return p[i]; }
  inline const T& operator()(int i, int j) const {
    return p[static_cast<std::size_t>(i) + static_cast<std::size_t>(nr) * static_cast<std::size_t>(j)];
  }
  inline std::size_t count() const { return len; }
  inline int size() const { return static_cast<int>(len); }
};
inline L2Vec<double> L2Vec_d(SEXP x) {
  L2Vec<double> o;
  o.len = static_cast<std::size_t>(Rf_xlength(x));
  o.nr = Rf_isMatrix(x) ? Rf_nrows(x) : static_cast<int>(o.len);
  o.nc = Rf_isMatrix(x) ? Rf_ncols(x) : 1;
  if (o.nr == 0) o.nc = 0;
  if (TYPEOF(x) == REALSXP) { if (o.len) o.p = REAL(x); }
  else { Rcpp::NumericVector r(x); o.v = std::make_shared< std::vector<double> >(r.begin(), r.end());
    o.p = o.v->data(); }
  return o;
}
inline L2Vec<int> L2Vec_i(SEXP x) {
  L2Vec<int> o;
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
inline L2Vec<double> l2_d(SEXP x) { return L2Vec_d(x); }
inline L2Vec<int> l2_i(SEXP x) { return L2Vec_i(x); }

struct L2Data {
  L2Vec<int> score, fixed_ability, A_ref, B_ref, C_ref, D_ref, free_mask;
  L2Vec<double> theta_mode, person_pred, fixed_ability_value, A_fixed_value, A_pred,
    B_fixed_value, B_pred, C_fixed_value, C_pred, D_fixed_value, D_pred,
    Abilitybeta, invspApars, invspAbeta, Bpars, Bbeta, logitCpars, logitCbeta, logitDpars, logitDbeta,
    prior_mean, prior_precision;
  int K = 1, P = 0;
  double kappa = 0;     // curvature floor, as a share of the Fisher information (off)
  double beta_term(int o, const L2Vec<double>& pred, const L2Vec<double>& beta, int brow) const {
    if (brow <= 0 || pred.nc == 0 || beta.nr == 0) return 0.0;
    double s = 0; for (int j = 0; j < pred.nc; ++j) s += pred(o, j) * beta(brow - 1, j);
    return s;
  }
};

struct L2Worker : public RcppParallel::Worker {
  const L2Data& D;
  const BigIRTObsIndex& obs_by_subj;
  double value = 0; int skipped = 0;
  std::vector<double> gA, gB, gC, gD, gAb, gBb, gCb, gDb, gAbil, gMean, gP0;
  L2Worker(const L2Data& D, const BigIRTObsIndex& obs) : D(D), obs_by_subj(obs) {
    gA.assign(D.invspApars.count(), 0); gB.assign(D.Bpars.count(), 0);
    gC.assign(D.logitCpars.count(), 0); gD.assign(D.logitDpars.count(), 0);
    gAb.assign(D.invspAbeta.count(), 0); gBb.assign(D.Bbeta.count(), 0);
    gCb.assign(D.logitCbeta.count(), 0); gDb.assign(D.logitDbeta.count(), 0);
    gAbil.assign(static_cast<std::size_t>(D.K) * D.P, 0); gMean.assign(D.K, 0);
    gP0.assign(static_cast<std::size_t>(D.K) * D.K, 0);
  }
  L2Worker(const L2Worker& o, RcppParallel::Split) : D(o.D), obs_by_subj(o.obs_by_subj) {
    gA.assign(o.gA.size(), 0); gB.assign(o.gB.size(), 0); gC.assign(o.gC.size(), 0);
    gD.assign(o.gD.size(), 0); gAb.assign(o.gAb.size(), 0); gBb.assign(o.gBb.size(), 0);
    gCb.assign(o.gCb.size(), 0); gDb.assign(o.gDb.size(), 0); gAbil.assign(o.gAbil.size(), 0);
    gMean.assign(o.gMean.size(), 0); gP0.assign(o.gP0.size(), 0);
  }
  void beta_grad(std::vector<double>& g, const L2Vec<double>& pred, const L2Vec<double>& beta,
                 int o, int brow, double v) {
    if (brow <= 0 || pred.nc == 0 || beta.nr == 0) return;
    for (int j = 0; j < pred.nc; ++j) g[static_cast<std::size_t>(brow - 1 + beta.nr * j)] += v * pred(o, j);
  }

  void operator()(std::size_t begin, std::size_t end) {
    const int K = D.K;
    for (std::size_t s = begin; s < end; ++s) {
      const int subj = static_cast<int>(s);
      const BigIRTObsSpan obs = obs_by_subj[subj];
      const int n = static_cast<int>(obs.size());
      if (n == 0) continue;
      std::vector<int> act;
      for (int k = 0; k < K; ++k) if (D.free_mask(subj, k) != 0) act.push_back(k);
      const int d = static_cast<int>(act.size());
      if (d == 0) continue;
      MatrixXd P0(K, K); VectorXd m(K), mu(K);
      for (int r = 0; r < K; ++r) { m(r) = D.theta_mode(subj, r); mu(r) = D.prior_mean(subj, r);
        for (int c = 0; c < K; ++c)
          P0(r, c) = D.prior_precision[static_cast<std::size_t>(r + K * c) + static_cast<std::size_t>(K) * K * subj]; }
      // Row pieces at the mode.
      MatrixXd A(n, d), TH(n, K), AM(n, K);   // active loadings; row ability; d loading / d raw
      MatrixXd Afull(n, K);
      std::vector<double> y(n), l1(n), l2(n), l3(n), l4(n), l5(n);
      std::vector<double> l1c(n, 0), l2c(n, 0), l3c(n, 0), l4c(n, 0), l1d(n, 0), l2d(n, 0), l3d(n, 0), l4d(n, 0);
      std::vector<double> cmul(n, 0), dmul(n, 0);
      // The curvature the correction uses, and, where it is floored, the
      // derivatives of the floor kappa w_F in eta, c and d.
      std::vector<double> wc(n, 0), fe(n, 0), fcv(n, 0), fdv(n, 0);
      std::vector<char> floored(n, 0);
      for (int oi = 0; oi < n; ++oi) {
        const int o = obs[oi];
        const int bb = D.B_ref[o] > 0 ? (D.Bbeta.nr == 1 ? 1 : D.B_ref[o]) : 0;
        const double b = D.B_ref[o] > 0 ? D.Bpars[D.B_ref[o] - 1] + D.beta_term(o, D.B_pred, D.Bbeta, bb) : D.B_fixed_value[o];
        double c = D.C_fixed_value[o], dd = D.D_fixed_value[o];
        if (D.C_ref[o] > 0) { const int cb = D.logitCbeta.nr == 1 ? 1 : D.C_ref[o];
          const double sg = l2_sig(D.logitCpars[D.C_ref[o] - 1] + D.beta_term(o, D.C_pred, D.logitCbeta, cb));
          c = 0.5 * sg; cmul[oi] = 0.5 * sg * (1 - sg); }
        if (D.D_ref[o] > 0) { const int db = D.logitDbeta.nr == 1 ? 1 : D.D_ref[o];
          const double sg = l2_sig(D.logitDpars[D.D_ref[o] - 1] + D.beta_term(o, D.D_pred, D.logitDbeta, db));
          dd = 0.5 * sg + 0.5; dmul[oi] = 0.5 * sg * (1 - sg); }
        double eta = -b;
        for (int k = 0; k < K; ++k) {
          double a = D.A_fixed_value(o, k), am = 0;
          if (D.A_ref(o, k) > 0) { const int abx = D.invspAbeta.nr == 1 ? 1 : D.A_ref(o, k);
            const double raw = D.invspApars[D.A_ref(o, k) - 1] + D.beta_term(o, D.A_pred, D.invspAbeta, abx);
            a = raw > 0 ? raw + std::log1p(std::exp(-raw)) : std::log1p(std::exp(raw)); am = l2_sig(raw); }
          double of = 0;
          if (D.fixed_ability(o, k) == 0) for (int j = 0; j < D.P; ++j) of += D.person_pred(o, j) * D.Abilitybeta(k, j);
          TH(oi, k) = D.fixed_ability(o, k) != 0 ? D.fixed_ability_value(o, k) : m(k) + of;
          Afull(oi, k) = a; AM(oi, k) = am;
          eta += a * TH(oi, k);
        }
        // The loadings the person's integral sees: free dimensions only, and
        // not where the row's ability is fixed.
        for (int k = 0; k < d; ++k) A(oi, k) = D.fixed_ability(o, act[k]) != 0 ? 0.0 : Afull(oi, act[k]);
        y[oi] = static_cast<double>(D.score[o]);
        double t[6]; l2_tower<double>(y[oi], eta, c, dd, t);
        l1[oi] = t[1]; l2[oi] = t[2]; l3[oi] = t[3]; l4[oi] = t[4]; l5[oi] = t[5];
        {
          // Fisher information in eta, w_F = u^2 s1^2 / (p (1 - p)).
          const double sg = l2_sig(eta), s1 = sg * (1 - sg), u = dd - c;
          double pp = c + u * sg; pp = std::min(std::max(pp, 1e-12), 1 - 1e-12);
          const double rr = std::max(pp * (1 - pp), 1e-12), q2 = u * u * s1 * s1;
          const double wF = q2 / rr;
          const double wobs = -t[2];
          if (D.kappa > 0 && wobs < D.kappa * wF) {
            wc[oi] = D.kappa * wF; floored[oi] = 1;
            fe[oi] = D.kappa * q2 * (2 * (1 - 2 * sg) * rr - u * s1 * (1 - 2 * pp)) / (rr * rr);
            fcv[oi] = D.kappa * (-2 * u * s1 * s1 * rr - q2 * (1 - sg) * (1 - 2 * pp)) / (rr * rr);
            fdv[oi] = D.kappa * (2 * u * s1 * s1 * rr - q2 * sg * (1 - 2 * pp)) / (rr * rr);
          } else wc[oi] = wobs;
        }
        if (D.C_ref[o] > 0) { Dual tc[6]; l2_tower<Dual>(y[oi], eta, Dual(c, 1), Dual(dd, 0), tc);
          l1c[oi] = tc[1].d; l2c[oi] = tc[2].d; l3c[oi] = tc[3].d; l4c[oi] = tc[4].d; }
        if (D.D_ref[o] > 0) { Dual td[6]; l2_tower<Dual>(y[oi], eta, Dual(c, 0), Dual(dd, 1), td);
          l1d[oi] = td[1].d; l2d[oi] = td[2].d; l3d[oi] = td[3].d; l4d[oi] = td[4].d; }
      }
      // H on the free dimensions; skip persons whose data curvature is not PSD.
      MatrixXd P0a(d, d); VectorXd dm(d);
      for (int r = 0; r < d; ++r) { dm(r) = m(act[r]) - mu(act[r]);
        for (int c = 0; c < d; ++c) P0a(r, c) = P0(act[r], act[c]); }
      // Two curvatures. The correction's Gaussian may use a floored one, wc
      // (options(bigIRT.laplace2_floor = kappa), off by default): with free
      // asymptotes the observed curvature of a response near the guessing
      // floor can be zero or negative, S then fails to shrink as the loadings
      // grow, and the E T3^2 term grows like a^6 -- a two-dimensional 3PL
      // refinement drove the objective to 1e13 that way. Flooring at kappa
      // times the Fisher information, which grows like a^2, bounds it, but
      // measured against 9^K optima (dev/optim/mdim_lowk.R) it left some
      // items 15 to 23 SE off at K = 2 and 3, where without it the K = 3 fit
      // was within 0.8 and the K = 2 one blew up and was reverted by
      // fitIRT's guard. Silently wrong is worse than caught, so it is off.
      // For the 2PL the two curvatures are equal and nothing is ever floored.
      // The mode adjoint needs the actual Hessian, so that is kept too.
      MatrixXd Dw = MatrixXd::Zero(d, d), Dt = MatrixXd::Zero(d, d);
      for (int oi = 0; oi < n; ++oi) {
        Dw += wc[oi] * A.row(oi).transpose() * A.row(oi);
        Dt += (-l2[oi]) * A.row(oi).transpose() * A.row(oi);
      }
      // Where the data curvature is not positive semidefinite, clip it to
      // the nearest matrix that is, as the Laplace path does. Clipping is
      // continuous, so the objective stays continuous across the boundary
      // (skipping these persons instead made their correction jump to zero
      // there, and stalled the line search on 3PL data); H is then locally
      // constant in the rows, exactly so at one dimension, and its
      // derivative terms are dropped.
      bool clipped = false;
      {
        Eigen::SelfAdjointEigenSolver<MatrixXd> es(Dw);
        if (es.eigenvalues().minCoeff() < 0) {
          VectorXd lam = es.eigenvalues().cwiseMax(0.0);
          Dw = es.eigenvectors() * lam.asDiagonal() * es.eigenvectors().transpose();
          clipped = true; ++skipped;
        }
      }
      const MatrixXd H = P0a + Dw;
      Eigen::LLT<MatrixXd> llt(H);
      if (llt.info() != Eigen::Success) { ++skipped; continue; }
      const MatrixXd S = llt.solve(MatrixXd::Identity(d, d));
      MatrixXd St = S;
      {
        Eigen::SelfAdjointEigenSolver<MatrixXd> es(Dt);
        if (es.eigenvalues().minCoeff() < 0)
          Dt = es.eigenvectors() * es.eigenvalues().cwiseMax(0.0).asDiagonal() * es.eigenvectors().transpose();
        Eigen::LLT<MatrixXd> lt(P0a + Dt);
        if (lt.info() == Eigen::Success) St = lt.solve(MatrixXd::Identity(d, d));
      }
      const MatrixXd Q = A * S;                  // n x d
      const MatrixXd C = Q * A.transpose();      // n x n
      VectorXd v(n), L3(n);
      for (int r = 0; r < n; ++r) { v(r) = l3[r] * C(r, r); L3(r) = l3[r]; }
      const VectorXd Cv = C * v;
      double val = 0;
      for (int r = 0; r < n; ++r) val += 0.125 * l4[r] * C(r, r) * C(r, r);
      val += 0.125 * v.dot(Cv);
      MatrixXd C2 = C.cwiseProduct(C);
      const MatrixXd C3 = C2.cwiseProduct(C);
      val += (1.0 / 12.0) * L3.dot(C3 * L3);
      value += val;
      // Reverse accumulation.
      MatrixXd Gbar = 0.125 * v * v.transpose() + 0.25 * (L3 * L3.transpose()).cwiseProduct(C2);
      for (int r = 0; r < n; ++r) Gbar(r, r) += 0.25 * l4[r] * C(r, r) + 0.25 * Cv(r) * l3[r];
      const VectorXd C3L3 = C3 * L3;
      std::vector<double> dl3(n), dl4(n), dl2(n);
      for (int r = 0; r < n; ++r) {
        dl3[r] = 0.25 * Cv(r) * C(r, r) + C3L3(r) / 6.0;
        dl4[r] = 0.125 * C(r, r) * C(r, r);
      }
      const MatrixXd AGA = A.transpose() * Gbar * A;
      const MatrixXd Hbar = -S * AGA * S;
      const MatrixXd GQ = Gbar * Q;              // n x d
      const MatrixXd AH = A * Hbar;              // n x d
      VectorXd gm = VectorXd::Zero(d);
      std::vector<double> geta(n), gc(n), gd(n);
      MatrixXd ga(n, d);
      for (int r = 0; r < n; ++r) {
        const double aHa = AH.row(r).dot(A.row(r));
        // dC/dw_r = a'Hbar a, through -l2 or through the floor.
        dl2[r] = (clipped || floored[r]) ? 0.0 : -aHa;   // H frozen where clipped
        geta[r] = dl2[r] * l3[r] + dl3[r] * l4[r] + dl4[r] * l5[r];
        gc[r] = dl2[r] * l2c[r] + dl3[r] * l3c[r] + dl4[r] * l4c[r];
        gd[r] = dl2[r] * l2d[r] + dl3[r] * l3d[r] + dl4[r] * l4d[r];
        if (floored[r] && !clipped) { geta[r] += aHa * fe[r]; gc[r] += aHa * fcv[r]; gd[r] += aHa * fdv[r]; }
        ga.row(r) = 2.0 * GQ.row(r);
        if (!clipped) ga.row(r) += 2.0 * wc[r] * AH.row(r);
        gm += geta[r] * A.row(r).transpose();
      }
      // Mode adjoint.
      const VectorXd u = St * gm;
      for (int r = 0; r < n; ++r) {
        const double au = A.row(r).dot(u);
        geta[r] += l2[r] * au;
        gc[r] += l1c[r] * au;
        gd[r] += l1d[r] * au;
        ga.row(r) += l1[r] * u.transpose();
      }
      // Scatter to parameters.
      for (int oi = 0; oi < n; ++oi) {
        const int o = obs[oi];
        // Loadings: d/d loading_k = direct (free dims) + geta * row ability.
        for (int k = 0; k < K; ++k) {
          double gl = geta[oi] * TH(oi, k);
          for (int kk = 0; kk < d; ++kk) if (act[kk] == k && D.fixed_ability(o, k) == 0) gl += ga(oi, kk);
          if (D.A_ref(o, k) > 0) {
            const double graw = gl * AM(oi, k);
            gA[static_cast<std::size_t>(D.A_ref(o, k) - 1)] += graw;
            beta_grad(gAb, D.A_pred, D.invspAbeta, o, D.invspAbeta.nr == 1 ? 1 : D.A_ref(o, k), graw);
          }
          if (D.fixed_ability(o, k) == 0)
            for (int j = 0; j < D.P; ++j)
              gAbil[static_cast<std::size_t>(k + K * j)] += geta[oi] * Afull(oi, k) * D.person_pred(o, j);
        }
        if (D.B_ref[o] > 0) {
          gB[static_cast<std::size_t>(D.B_ref[o] - 1)] -= geta[oi];
          beta_grad(gBb, D.B_pred, D.Bbeta, o, D.Bbeta.nr == 1 ? 1 : D.B_ref[o], -geta[oi]);
        }
        if (D.C_ref[o] > 0) {
          const double g = gc[oi] * cmul[oi];
          gC[static_cast<std::size_t>(D.C_ref[o] - 1)] += g;
          beta_grad(gCb, D.C_pred, D.logitCbeta, o, D.logitCbeta.nr == 1 ? 1 : D.C_ref[o], g);
        }
        if (D.D_ref[o] > 0) {
          const double g = gd[oi] * dmul[oi];
          gD[static_cast<std::size_t>(D.D_ref[o] - 1)] += g;
          beta_grad(gDb, D.D_pred, D.logitDbeta, o, D.logitDbeta.nr == 1 ? 1 : D.D_ref[o], g);
        }
      }
      // Ability mean: the free modes move with mu through -P0 (theta - mu),
      // whose free rows reach every dimension of mu.
      for (int j = 0; j < K; ++j) {
        double s_ = 0; for (int i = 0; i < d; ++i) s_ += u(i) * P0(act[i], j);
        gMean[j] += s_;
      }
      // Prior precision: directly through H (free block), and through the
      // mode, whose condition has -P0[free, ] (theta - mu).
      const VectorXd dmf = m - mu;
      for (int r = 0; r < d; ++r) for (int c = 0; c < d; ++c)
        gP0[static_cast<std::size_t>(act[r] + K * act[c])] += Hbar(r, c);
      for (int i = 0; i < d; ++i) for (int j = 0; j < K; ++j) {
        const double g_ = -0.5 * u(i) * dmf(j);
        gP0[static_cast<std::size_t>(act[i] + K * j)] += g_;
        gP0[static_cast<std::size_t>(j + K * act[i])] += g_;
      }
    }
  }
  void join(const L2Worker& o) {
    value += o.value; skipped += o.skipped;
    auto add = [](std::vector<double>& a, const std::vector<double>& b) {
      for (std::size_t i = 0; i < a.size(); ++i) a[i] += b[i]; };
    add(gA, o.gA); add(gB, o.gB); add(gC, o.gC); add(gD, o.gD); add(gAb, o.gAb); add(gBb, o.gBb);
    add(gCb, o.gCb); add(gDb, o.gDb); add(gAbil, o.gAbil); add(gMean, o.gMean); add(gP0, o.gP0);
  }
};

} // namespace

extern "C" SEXP _bigIRT_laplace2_fg_cpp_impl(
    SEXP idSEXP, SEXP scoreSEXP, SEXP theta_modeSEXP,
    SEXP person_predSEXP, SEXP fixed_abilitySEXP, SEXP fixed_ability_valueSEXP,
    SEXP A_refSEXP, SEXP A_fixed_valueSEXP, SEXP A_predSEXP,
    SEXP B_refSEXP, SEXP B_fixed_valueSEXP, SEXP B_predSEXP,
    SEXP C_refSEXP, SEXP C_fixed_valueSEXP, SEXP C_predSEXP,
    SEXP D_refSEXP, SEXP D_fixed_valueSEXP, SEXP D_predSEXP,
    SEXP AbilitybetaSEXP, SEXP invspAparsSEXP, SEXP invspAbetaSEXP, SEXP BparsSEXP, SEXP BbetaSEXP,
    SEXP logitCparsSEXP, SEXP logitCbetaSEXP, SEXP logitDparsSEXP, SEXP logitDbetaSEXP,
    SEXP prior_meanSEXP, SEXP prior_precisionSEXP, SEXP free_maskSEXP, SEXP kappaSEXP, SEXP grainSEXP) {
  BEGIN_RCPP
  Rcpp::IntegerVector id(idSEXP);
  Rcpp::NumericMatrix theta_mode(theta_modeSEXP);
  const int Nobs = id.size(), N = theta_mode.nrow(), K = theta_mode.ncol();
  L2Data D;
  D.score = l2_i(scoreSEXP); D.fixed_ability = l2_i(fixed_abilitySEXP);
  D.A_ref = l2_i(A_refSEXP); D.B_ref = l2_i(B_refSEXP); D.C_ref = l2_i(C_refSEXP); D.D_ref = l2_i(D_refSEXP);
  D.free_mask = l2_i(free_maskSEXP);
  D.theta_mode = l2_d(theta_modeSEXP); D.person_pred = l2_d(person_predSEXP);
  D.fixed_ability_value = l2_d(fixed_ability_valueSEXP);
  D.A_fixed_value = l2_d(A_fixed_valueSEXP); D.A_pred = l2_d(A_predSEXP);
  D.B_fixed_value = l2_d(B_fixed_valueSEXP); D.B_pred = l2_d(B_predSEXP);
  D.C_fixed_value = l2_d(C_fixed_valueSEXP); D.C_pred = l2_d(C_predSEXP);
  D.D_fixed_value = l2_d(D_fixed_valueSEXP); D.D_pred = l2_d(D_predSEXP);
  D.Abilitybeta = l2_d(AbilitybetaSEXP); D.invspApars = l2_d(invspAparsSEXP); D.invspAbeta = l2_d(invspAbetaSEXP);
  D.Bpars = l2_d(BparsSEXP); D.Bbeta = l2_d(BbetaSEXP); D.logitCpars = l2_d(logitCparsSEXP);
  D.logitCbeta = l2_d(logitCbetaSEXP); D.logitDpars = l2_d(logitDparsSEXP); D.logitDbeta = l2_d(logitDbetaSEXP);
  D.prior_mean = l2_d(prior_meanSEXP); D.prior_precision = l2_d(prior_precisionSEXP);
  D.K = K;
  D.P = (D.person_pred.nc > 0 && D.Abilitybeta.nc == D.person_pred.nc) ? D.person_pred.nc : 0;
  BigIRTObsIndex obs_by_subj;
  if (!obs_by_subj.build(id, Nobs, N)) Rcpp::stop("id must be coded from 1 to Nsubs.");
  D.kappa = Rcpp::as<double>(kappaSEXP);
  L2Worker w(D, obs_by_subj);
  RcppParallel::parallelReduce(static_cast<std::size_t>(0), static_cast<std::size_t>(N), w,
                               static_cast<std::size_t>(std::max(1, Rcpp::as<int>(grainSEXP))));
  auto asv = [](const std::vector<double>& v) { return Rcpp::NumericVector(v.begin(), v.end()); };
  Rcpp::NumericMatrix gAbil(K, D.P), gP0(K, K);
  std::copy(w.gAbil.begin(), w.gAbil.end(), gAbil.begin());
  std::copy(w.gP0.begin(), w.gP0.end(), gP0.begin());
  return Rcpp::List::create(
    Rcpp::Named("value") = w.value, Rcpp::Named("skipped") = w.skipped,
    Rcpp::Named("grad_A") = asv(w.gA), Rcpp::Named("grad_B") = asv(w.gB),
    Rcpp::Named("grad_C") = asv(w.gC), Rcpp::Named("grad_D") = asv(w.gD),
    Rcpp::Named("grad_A_beta") = asv(w.gAb), Rcpp::Named("grad_B_beta") = asv(w.gBb),
    Rcpp::Named("grad_C_beta") = asv(w.gCb), Rcpp::Named("grad_D_beta") = asv(w.gDb),
    Rcpp::Named("grad_ability_beta") = gAbil, Rcpp::Named("grad_mean") = asv(w.gMean),
    Rcpp::Named("grad_P0") = gP0);
  END_RCPP
}
