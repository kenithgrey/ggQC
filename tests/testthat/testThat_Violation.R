require(dplyr)
require(tidyr)

#path <- "tests/testthat/wheeler91.csv"
path <- "wheeler91.csv"
wheeler91 <- read.csv(path, header=T)
Wheeler91_QC_Test <- QC_Violations(data = wheeler91$Average, method = "XmR") # No Error

#write.csv(Wheeler91_QC_Test, file = "Wheeler91_QC_Result.csv", quote = F, row.names = F)
Wheeler91_QC_Result <- read.csv("Wheeler91_QC_Result.csv", header=T, stringsAsFactors = F)

# Wheeler Test Data With Grouping -----------------------------------------
    # This uses the error in the triplicat measurments.
    wheeler91 %>%
      select(Subgroup, Measurement.1, Measurement.2, Measurement.3) %>%
      gather(Temp, "Measurment", 2:4) ->
      Wheeler_91_LongForm

#colnames(Wheeler_91_LongForm)
#QC_Lines(Wheeler_91_LongForm, formula = Measurment~Subgroup, method = "xBar.rBar")
#write.csv(Wheeler_91_LongForm_QC, file = "Wheeler_91_LongForm_QC_Grouping_Violation.csv", quote = F, row.names = F)
Wheeler_91_Grouping_Violation_Reference_Result <- read.csv("Wheeler_91_LongForm_QC_Grouping_Violation.csv", stringsAsFactors = F)

Wheeler_91_LongForm_QC_Test <-
  QC_Violations(Wheeler_91_LongForm,
                formula = Measurment~Subgroup,
                method = "xBar.rBar")

context("Violation Functions")
testthat::test_that("warnings tripped for n < 20", {
  testthat::expect_warning(QC_Violations(data = wheeler91$Average, method = "XMR"))
  testthat::expect_equal(Wheeler91_QC_Test, Wheeler91_QC_Result , tolerance = .001, scale = 1)
  testthat::expect_equal(Wheeler_91_LongForm_QC_Test, Wheeler_91_Grouping_Violation_Reference_Result , tolerance = .001, scale = 1)
})



