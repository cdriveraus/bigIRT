// Item blocks over an item's covariate coefficients as well as its slots.
//
// The item blocks of the Hessian (src/agq_backend.cpp, src/map_backend.cpp;
// read by the certificate's fast route, R/certify.R) are over each item's
// slots -- B, the loading on each of the K dimensions, C, D -- followed by its
// item-covariate coefficients: the B covariates, the A covariates for each
// dimension in turn, the C covariates, then the D covariates. With
// itemSpecificBetas those coefficients are the item's own, and leaving them
// out of its block made the certificate difference every one of them: two
// evaluations each, nearly 6,000 on a 500-item fit with three covariates on A
// and B, where the fit itself took a few seconds and the certificate more than
// ten minutes.
//
// A coefficient moves only its own slot's linear predictor, by the row's
// covariate value, so with J_r = [I | X_r] the Jacobian of row r's slot
// predictors in the item's coordinates, row r adds J_r' M_r J_r to its item's
// block, M_r being the S x S matrix the kernels form in the slots. J_r is
// applied row by row, which keeps it exact when a covariate varies within an
// item; it does not depend on the ability, so it is the same at every node.
// The kernels fill these columns whenever a slot has covariates; R reads them
// only for item-specific coefficients, a shared coefficient's curvature
// summing over every item.

#ifndef BIGIRT_ITEM_BLOCKS_H
#define BIGIRT_ITEM_BLOCKS_H

#include <vector>

struct BigirtBlockLayout {
  int S = 0, E = 0;
  std::vector<int> slot;   // the slot each coefficient column moves

  void init(int K, int nB, int nA, int nC, int nD) {
    S = K + 3;
    slot.clear();
    for (int j = 0; j < nB; ++j) slot.push_back(0);
    for (int k = 0; k < K; ++k) for (int j = 0; j < nA; ++j) slot.push_back(1 + k);
    for (int j = 0; j < nC; ++j) slot.push_back(K + 1);
    for (int j = 0; j < nD; ++j) slot.push_back(K + 2);
    E = S + static_cast<int>(slot.size());
  }

  // Row o's covariate values in the coefficient columns, zero where the row
  // does not free the slot (the kernels' convention: such a slot's score and
  // Hessian rows are zero anyway).
  template <class IV, class DV>
  void covariates(int o, int K, const IV& A_ref, const IV& B_ref, const IV& C_ref, const IV& D_ref,
                  const DV& A_pred, const DV& B_pred, const DV& C_pred, const DV& D_pred,
                  double* xv) const {
    int c = 0;
    for (int j = 0; j < B_pred.nc; ++j, ++c) xv[c] = B_ref[o] > 0 ? B_pred(o, j) : 0.0;
    for (int k = 0; k < K; ++k)
      for (int j = 0; j < A_pred.nc; ++j, ++c) xv[c] = A_ref(o, k) > 0 ? A_pred(o, j) : 0.0;
    for (int j = 0; j < C_pred.nc; ++j, ++c) xv[c] = C_ref[o] > 0 ? C_pred(o, j) : 0.0;
    for (int j = 0; j < D_pred.nc; ++j, ++c) xv[c] = D_ref[o] > 0 ? D_pred(o, j) : 0.0;
  }

  // B (E x E, column major) += J' M J, M symmetric S x S.
  void add(double* B, const double* M, const double* xv) const {
    for (int b = 0; b < S; ++b) for (int a = 0; a < S; ++a) B[a + E * b] += M[a + S * b];
    const int nb = E - S;
    for (int c = 0; c < nb; ++c) {
      const double xc = xv[c];
      if (xc == 0.0) continue;
      const int sc = slot[c];
      for (int a = 0; a < S; ++a) {
        const double v = M[a + S * sc] * xc;
        B[a + E * (S + c)] += v;
        B[(S + c) + E * a] += v;
      }
      for (int d = 0; d < nb; ++d) {
        if (xv[d] == 0.0) continue;
        B[(S + c) + E * (S + d)] += xc * M[sc + S * slot[d]] * xv[d];
      }
    }
  }

  // A d x S matrix of cross derivatives (row-major by dimension, stride S) to
  // d x E (stride E): X J.
  void extend_rows(const double* Xs, int d, const double* xv, double* Xe) const {
    const int nb = E - S;
    for (int a = 0; a < d; ++a) {
      for (int s = 0; s < S; ++s) Xe[a * E + s] = Xs[a * S + s];
      for (int c = 0; c < nb; ++c) Xe[a * E + S + c] = Xs[a * S + slot[c]] * xv[c];
    }
  }
};

#endif
