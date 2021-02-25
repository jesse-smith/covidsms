# The `var` argument of `postmastr::pm_identify()` requires a data-masked
# variablei as input.
# Suppress "no visible binding for global variable" for this input
if (getRversion() >= "2.15.1") utils::globalVariables(".ltcf_addr1_tmp_")
