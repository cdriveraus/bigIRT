/* Registration of the package's native routines.
 *
 * Every kernel is an extern "C" function called from R by a literal
 * .Call(`_bigIRT_...`), so this table is what R resolves those names against.
 * It is maintained by hand: add an entry, with its argument count, whenever a
 * routine is added or its arguments change. To check it against the R code,
 *   tools::package_native_routine_registration_skeleton(".")
 * prints the table the .Call sites imply; the two should agree.
 */
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP _bigIRT_agq_fg_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_fisher_blocks_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_fisher_dense_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_item_info_accum_cpp_impl(SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_laplace_ability_beta_rows_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_laplace_clip_counts_cpp_impl(SEXP);
extern SEXP _bigIRT_laplace_corr_grad_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_laplace_direct_block_fg_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_laplace_item_block_objective_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_laplace_item_objective_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_laplace_person_beta_fused_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_laplace_person_beta_grouped_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_laplace_person_row_terms_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_laplace_person_step_block_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_laplace_row_terms_debug_cpp_impl(SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_laplace2_fg_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_map_fg_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_person_covariance_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _bigIRT_row_aSa_cpp_impl(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_bigIRT_agq_fg_cpp_impl",                       (DL_FUNC) &_bigIRT_agq_fg_cpp_impl,                       49},
    {"_bigIRT_fisher_blocks_cpp_impl",                (DL_FUNC) &_bigIRT_fisher_blocks_cpp_impl,                43},
    {"_bigIRT_fisher_dense_cpp_impl",                 (DL_FUNC) &_bigIRT_fisher_dense_cpp_impl,                 43},
    {"_bigIRT_item_info_accum_cpp_impl",              (DL_FUNC) &_bigIRT_item_info_accum_cpp_impl,               4},
    {"_bigIRT_laplace_ability_beta_rows_cpp_impl",    (DL_FUNC) &_bigIRT_laplace_ability_beta_rows_cpp_impl,    13},
    {"_bigIRT_laplace_clip_counts_cpp_impl",          (DL_FUNC) &_bigIRT_laplace_clip_counts_cpp_impl,           1},
    {"_bigIRT_laplace_corr_grad_cpp_impl",            (DL_FUNC) &_bigIRT_laplace_corr_grad_cpp_impl,             9},
    {"_bigIRT_laplace_direct_block_fg_cpp_impl",      (DL_FUNC) &_bigIRT_laplace_direct_block_fg_cpp_impl,      42},
    {"_bigIRT_laplace_item_block_objective_cpp_impl", (DL_FUNC) &_bigIRT_laplace_item_block_objective_cpp_impl, 36},
    {"_bigIRT_laplace_item_objective_cpp_impl",       (DL_FUNC) &_bigIRT_laplace_item_objective_cpp_impl,       11},
    {"_bigIRT_laplace_person_beta_fused_cpp_impl",    (DL_FUNC) &_bigIRT_laplace_person_beta_fused_cpp_impl,    13},
    {"_bigIRT_laplace_person_beta_grouped_cpp_impl",  (DL_FUNC) &_bigIRT_laplace_person_beta_grouped_cpp_impl,  14},
    {"_bigIRT_laplace_person_row_terms_cpp_impl",     (DL_FUNC) &_bigIRT_laplace_person_row_terms_cpp_impl,      9},
    {"_bigIRT_laplace_person_step_block_cpp_impl",    (DL_FUNC) &_bigIRT_laplace_person_step_block_cpp_impl,    41},
    {"_bigIRT_laplace_row_terms_debug_cpp_impl",      (DL_FUNC) &_bigIRT_laplace_row_terms_debug_cpp_impl,       4},
    {"_bigIRT_laplace2_fg_cpp_impl",                  (DL_FUNC) &_bigIRT_laplace2_fg_cpp_impl,                  33},
    {"_bigIRT_map_fg_cpp_impl",                       (DL_FUNC) &_bigIRT_map_fg_cpp_impl,                       39},
    {"_bigIRT_person_covariance_cpp_impl",            (DL_FUNC) &_bigIRT_person_covariance_cpp_impl,            12},
    {"_bigIRT_row_aSa_cpp_impl",                      (DL_FUNC) &_bigIRT_row_aSa_cpp_impl,                       5},
    {NULL, NULL, 0}
};

void R_init_bigIRT(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
