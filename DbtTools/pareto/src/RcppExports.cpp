// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// c_inPSphere2D
IntegerVector c_inPSphere2D(NumericMatrix data, IntegerVector xBinNr, IntegerVector yBinNr, unsigned int nrXBins, unsigned int nrYBins, unsigned int nrData, double paretoRadius);
RcppExport SEXP _pareto_c_inPSphere2D(SEXP dataSEXP, SEXP xBinNrSEXP, SEXP yBinNrSEXP, SEXP nrXBinsSEXP, SEXP nrYBinsSEXP, SEXP nrDataSEXP, SEXP paretoRadiusSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type xBinNr(xBinNrSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yBinNr(yBinNrSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nrXBins(nrXBinsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nrYBins(nrYBinsSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nrData(nrDataSEXP);
    Rcpp::traits::input_parameter< double >::type paretoRadius(paretoRadiusSEXP);
    rcpp_result_gen = Rcpp::wrap(c_inPSphere2D(data, xBinNr, yBinNr, nrXBins, nrYBins, nrData, paretoRadius));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_pareto_c_inPSphere2D", (DL_FUNC) &_pareto_c_inPSphere2D, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_pareto(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
