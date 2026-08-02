#include <TMB.hpp>

// Deliberately private feasibility model.  It covers the core sparse IRT
// likelihood so that TMB's Laplace value and automatic derivative can be
// compared to the package backends before TMB becomes a package dependency.
template<class Type>
Type objective_function<Type>::operator() () {
  DATA_IVECTOR(id);          // zero-based person index
  DATA_IVECTOR(item);        // zero-based item index
  DATA_IVECTOR(score);
  DATA_INTEGER(n_person);
  DATA_INTEGER(n_item);
  DATA_INTEGER(n_scale);
  DATA_INTEGER(pl);
  DATA_SCALAR(ability_sd);
  DATA_SCALAR(item_prior_sd);

  PARAMETER_MATRIX(theta);   // n_person x n_scale; integrated by TMB
  PARAMETER_MATRIX(alpha);   // unconstrained discrimination parameters
  PARAMETER_VECTOR(b);
  PARAMETER_VECTOR(gamma);
  PARAMETER_VECTOR(delta);

  Type nll = Type(0);
  for (int person = 0; person < n_person; ++person) {
    for (int scale = 0; scale < n_scale; ++scale) {
      nll -= dnorm(theta(person, scale), Type(0), ability_sd, true);
    }
  }
  for (int it = 0; it < n_item; ++it) {
    nll -= dnorm(b(it), Type(0), item_prior_sd, true);
    for (int scale = 0; scale < n_scale; ++scale) {
      nll -= dnorm(alpha(it, scale), Type(0), item_prior_sd, true);
    }
    if (pl >= 3) nll -= dnorm(gamma(it), Type(-4), item_prior_sd, true);
    if (pl >= 4) nll -= dnorm(delta(it), Type(4), item_prior_sd, true);
  }

  for (int row = 0; row < score.size(); ++row) {
    Type eta = -b(item(row));
    for (int scale = 0; scale < n_scale; ++scale) {
      const Type loading = log(Type(1) + exp(alpha(item(row), scale)));
      eta += loading * theta(id(row), scale);
    }
    const Type logistic = invlogit(eta);
    const Type c = pl >= 3 ? Type(0.5) * invlogit(gamma(item(row))) : Type(0);
    const Type d = pl >= 4 ? Type(0.5) + Type(0.5) * invlogit(delta(item(row))) : Type(1);
    const Type probability = CppAD::CondExpLt(c + (d - c) * logistic, Type(1e-12), Type(1e-12),
      CppAD::CondExpGt(c + (d - c) * logistic, Type(1) - Type(1e-12), Type(1) - Type(1e-12), c + (d - c) * logistic));
    nll -= score(row) ? log(probability) : log(Type(1) - probability);
  }
  return nll;
}
