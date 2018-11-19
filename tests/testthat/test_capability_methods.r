df <- read.csv("wheeler_USPC_118.csv")


# For Reference QC_Line Ouptut --------------------------------------------
XmR <- QC_Lines(data=df$Value, method="XmR")
XmR.ordered <- QC_Lines(data=df$Value[order(df$Run)], method="XmR")

Student1 <- QC_Lines(data = df, formula = Value ~ Run)
Student2 <- QC_Lines(data = df, formula = Value ~ Run, method = "xBar.sBar")



# Manual Capability Test  -------------------------------------------------
Manual_Capa_Xbar <- capability.summary(LSL = 0, USL = 15,
                                       QC.Center = 4.66,
                                       QC.Sigma = 1.80,
                                       s.Sigma = sd(df$Value)
)


# Auto Capability Test ----------------------------------------------------
Auto_Capa_Xbar <-
  QC_Capability(data=df, value="Value", grouping = "Run", LSL=0, USL = 15)
Auto_Capa_XmR <- QC_Capability(data=df$Value[order(df$Run)],LSL=0, USL = 15, method="XmR")

Capa_df <- Reduce(f = function(x,y){merge(x,y, by="label")},
                  list(Manual_Capa_Xbar, Auto_Capa_Xbar, Auto_Capa_XmR))

colnames(Capa_df) <- c("label", "Man_xBar", "Auto_xBar", "Auto_XmR")
#write.csv(x = Capa_df, file = "tests/testthat/Wheeler118_result.csv", row.names = F)
Capa_Ref <- read.csv("Wheeler118_result.csv")


# The Test ----------------------------------------------------------------
context("Capability Functions")
testthat::test_that("Capability Methods Working", {
  expect_equal(Capa_df, Capa_Ref, tolerance = .01, scale = 1)
})
