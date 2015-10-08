// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// serialize_cookie
SEXP serialize_cookie();
RcppExport SEXP BWPMF_serialize_cookie() {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    __result = Rcpp::wrap(serialize_cookie());
    return __result;
END_RCPP
}
// deserialize_cookie
void deserialize_cookie(RawVector src);
RcppExport SEXP BWPMF_deserialize_cookie(SEXP srcSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< RawVector >::type src(srcSEXP);
    deserialize_cookie(src);
    return R_NilValue;
END_RCPP
}
// serialize_hostname
SEXP serialize_hostname();
RcppExport SEXP BWPMF_serialize_hostname() {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    __result = Rcpp::wrap(serialize_hostname());
    return __result;
END_RCPP
}
// deserialize_hostname
void deserialize_hostname(RawVector src);
RcppExport SEXP BWPMF_deserialize_hostname(SEXP srcSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< RawVector >::type src(srcSEXP);
    deserialize_hostname(src);
    return R_NilValue;
END_RCPP
}
// query_cookie
SEXP query_cookie(CharacterVector cookie);
RcppExport SEXP BWPMF_query_cookie(SEXP cookieSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< CharacterVector >::type cookie(cookieSEXP);
    __result = Rcpp::wrap(query_cookie(cookie));
    return __result;
END_RCPP
}
// query_hostname
SEXP query_hostname(CharacterVector hostname);
RcppExport SEXP BWPMF_query_hostname(SEXP hostnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< CharacterVector >::type hostname(hostnameSEXP);
    __result = Rcpp::wrap(query_hostname(hostname));
    return __result;
END_RCPP
}
// count_cookie
SEXP count_cookie();
RcppExport SEXP BWPMF_count_cookie() {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    __result = Rcpp::wrap(count_cookie());
    return __result;
END_RCPP
}
// count_hostname
SEXP count_hostname();
RcppExport SEXP BWPMF_count_hostname() {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    __result = Rcpp::wrap(count_hostname());
    return __result;
END_RCPP
}
// clean_cookie
void clean_cookie();
RcppExport SEXP BWPMF_clean_cookie() {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    clean_cookie();
    return R_NilValue;
END_RCPP
}
// clean_hostname
void clean_hostname();
RcppExport SEXP BWPMF_clean_hostname() {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    clean_hostname();
    return R_NilValue;
END_RCPP
}
// encode
void encode(const std::string& path, double progress);
RcppExport SEXP BWPMF_encode(SEXP pathSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const std::string& >::type path(pathSEXP);
    Rcpp::traits::input_parameter< double >::type progress(progressSEXP);
    encode(path, progress);
    return R_NilValue;
END_RCPP
}
// encode_data
SEXP encode_data(const std::string& path, double progress);
RcppExport SEXP BWPMF_encode_data(SEXP pathSEXP, SEXP progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const std::string& >::type path(pathSEXP);
    Rcpp::traits::input_parameter< double >::type progress(progressSEXP);
    __result = Rcpp::wrap(encode_data(path, progress));
    return __result;
END_RCPP
}
// serialize_history
SEXP serialize_history(SEXP Rhistory);
RcppExport SEXP BWPMF_serialize_history(SEXP RhistorySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rhistory(RhistorySEXP);
    __result = Rcpp::wrap(serialize_history(Rhistory));
    return __result;
END_RCPP
}
// deserialize_history
SEXP deserialize_history(RawVector src);
RcppExport SEXP BWPMF_deserialize_history(SEXP srcSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< RawVector >::type src(srcSEXP);
    __result = Rcpp::wrap(deserialize_history(src));
    return __result;
END_RCPP
}
// print_history
void print_history(SEXP Rhistory);
RcppExport SEXP BWPMF_print_history(SEXP RhistorySEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rhistory(RhistorySEXP);
    print_history(Rhistory);
    return R_NilValue;
END_RCPP
}
// check_history
NumericVector check_history(SEXP Rhistory);
RcppExport SEXP BWPMF_check_history(SEXP RhistorySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rhistory(RhistorySEXP);
    __result = Rcpp::wrap(check_history(Rhistory));
    return __result;
END_RCPP
}
// count_non_zero_of_history
SEXP count_non_zero_of_history(SEXP Rhistory);
RcppExport SEXP BWPMF_count_non_zero_of_history(SEXP RhistorySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rhistory(RhistorySEXP);
    __result = Rcpp::wrap(count_non_zero_of_history(Rhistory));
    return __result;
END_RCPP
}
// count_cookie_history
size_t count_cookie_history(SEXP Rhistory);
RcppExport SEXP BWPMF_count_cookie_history(SEXP RhistorySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rhistory(RhistorySEXP);
    __result = Rcpp::wrap(count_cookie_history(Rhistory));
    return __result;
END_RCPP
}
// count_hostname_history
size_t count_hostname_history(SEXP Rhistory);
RcppExport SEXP BWPMF_count_hostname_history(SEXP RhistorySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rhistory(RhistorySEXP);
    __result = Rcpp::wrap(count_hostname_history(Rhistory));
    return __result;
END_RCPP
}
// extract_history
SEXP extract_history(SEXP Rhistory, NumericVector id);
RcppExport SEXP BWPMF_extract_history(SEXP RhistorySEXP, SEXP idSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rhistory(RhistorySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type id(idSEXP);
    __result = Rcpp::wrap(extract_history(Rhistory, id));
    return __result;
END_RCPP
}
// compute_inverted_index
SEXP compute_inverted_index(SEXP Rmodel, SEXP Rhistory);
RcppExport SEXP BWPMF_compute_inverted_index(SEXP RmodelSEXP, SEXP RhistorySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rmodel(RmodelSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Rhistory(RhistorySEXP);
    __result = Rcpp::wrap(compute_inverted_index(Rmodel, Rhistory));
    return __result;
END_RCPP
}
// init_phi
void init_phi(SEXP Rmodel, SEXP Rtraining_history);
RcppExport SEXP BWPMF_init_phi(SEXP RmodelSEXP, SEXP Rtraining_historySEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rmodel(RmodelSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Rtraining_history(Rtraining_historySEXP);
    init_phi(Rmodel, Rtraining_history);
    return R_NilValue;
END_RCPP
}
// train_once
void train_once(SEXP Rmodel, SEXP Rtraining_history, SEXP Rtesting_history, SEXP Ritem_inverted_index);
RcppExport SEXP BWPMF_train_once(SEXP RmodelSEXP, SEXP Rtraining_historySEXP, SEXP Rtesting_historySEXP, SEXP Ritem_inverted_indexSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rmodel(RmodelSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Rtraining_history(Rtraining_historySEXP);
    Rcpp::traits::input_parameter< SEXP >::type Rtesting_history(Rtesting_historySEXP);
    Rcpp::traits::input_parameter< SEXP >::type Ritem_inverted_index(Ritem_inverted_indexSEXP);
    train_once(Rmodel, Rtraining_history, Rtesting_history, Ritem_inverted_index);
    return R_NilValue;
END_RCPP
}
// train
SEXP train(SEXP Rmodel, SEXP Rtraining_history, SEXP Rtesting_history, SEXP Ritem_inverted_index, int iteration);
RcppExport SEXP BWPMF_train(SEXP RmodelSEXP, SEXP Rtraining_historySEXP, SEXP Rtesting_historySEXP, SEXP Ritem_inverted_indexSEXP, SEXP iterationSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rmodel(RmodelSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Rtraining_history(Rtraining_historySEXP);
    Rcpp::traits::input_parameter< SEXP >::type Rtesting_history(Rtesting_historySEXP);
    Rcpp::traits::input_parameter< SEXP >::type Ritem_inverted_index(Ritem_inverted_indexSEXP);
    Rcpp::traits::input_parameter< int >::type iteration(iterationSEXP);
    __result = Rcpp::wrap(train(Rmodel, Rtraining_history, Rtesting_history, Ritem_inverted_index, iteration));
    return __result;
END_RCPP
}
// pmf_logloss
double pmf_logloss(SEXP Rmodel, SEXP Rhistory);
RcppExport SEXP BWPMF_pmf_logloss(SEXP RmodelSEXP, SEXP RhistorySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rmodel(RmodelSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Rhistory(RhistorySEXP);
    __result = Rcpp::wrap(pmf_logloss(Rmodel, Rhistory));
    return __result;
END_RCPP
}
// pmf_mae
double pmf_mae(SEXP Rmodel, SEXP Rhistory);
RcppExport SEXP BWPMF_pmf_mae(SEXP RmodelSEXP, SEXP RhistorySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type Rmodel(RmodelSEXP);
    Rcpp::traits::input_parameter< SEXP >::type Rhistory(RhistorySEXP);
    __result = Rcpp::wrap(pmf_mae(Rmodel, Rhistory));
    return __result;
END_RCPP
}
