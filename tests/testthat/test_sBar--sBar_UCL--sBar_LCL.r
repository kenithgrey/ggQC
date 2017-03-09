require(rQC)
# Desipersion Central Limit Tests -----------------------------------------


# Setup -------------------------------------------------------------------
web_data <- read.table("minitabTestFile_SBAR.csv", sep=",", header=T)
#web_data

# sBar Test Single Data Vector --------------------------------------------
WebData_V_G_sBarLCL <- sBar_LCL(web_data, value = "values", grouping = "groups") # 0
WebData_V_G_sBar <- sBar(web_data, value = "values", grouping = "groups") # 0.00204
WebData_V_G_sBarUCL <- sBar_UCL(web_data, value = "values", grouping = "groups") # 0.00427 (percisition .0001)

context("sBar -- sBar_UCL -- sBar_LCL")
testthat::test_that("sBar sBar_UCL sBar_LCL work using Value and Group", {
  expect_equal(WebData_V_G_sBarLCL, 0, tolerance = .01, scale = 1)
  expect_equal(WebData_V_G_sBar, 0.00204, tolerance = .0001, scale = 1)
  expect_equal(WebData_V_G_sBarUCL, 0.00427, tolerance = .0001, scale = 1)
})



WebData_formula_sBarLCL <- sBar_LCL(web_data, formula = values~groups) # 0
WebData_formula_sBar <- sBar(web_data, formula = values~groups) # 0.00204
WebData_formula_sBarUCL <- sBar_UCL(web_data, formula = values~groups) # 0.00427

testthat::test_that("sBar sBar_UCL sBar_LCL work using formula", {
  expect_equal(WebData_formula_sBarLCL, 0, tolerance = .01, scale = 1)
  expect_equal(WebData_formula_sBar, 0.00204, tolerance = .0001, scale = 1)
  expect_equal(WebData_formula_sBarUCL, 0.00427, tolerance = .0001, scale = 1)
})


# Rbar Test Single Data Vector --------------------------------------------
# The aggregate function seems to be robust to the missing data
# The effect of missing data here is that it will lower n to the lowest
# average integer 3 instead of 4 opening up the limits.
web_data_missing <- web_data
web_data_missing$values[1] <- NA
  #aggregate(values~groups, FUN = "length", data = web_data_missing)
  #aggregate(values~groups, FUN = "QCrange", data = web_data_missing)

WebData_missing_V_G_sBarLCL <- sBar_LCL(web_data_missing, value = "values", grouping = "groups") # 0
WebData_missing_V_G_sBar <- sBar(web_data_missing, value = "values", grouping = "groups") # 0.00207
WebData_missing_V_G_uBarLCL <- sBar_UCL(web_data_missing, value = "values", grouping = "groups") # 0.0047
testthat::test_that("sBar sBar_UCL sBar_LCL missing work using group and value", {
  expect_equal(WebData_missing_V_G_sBarLCL, 0, tolerance = .01, scale = 1)
  expect_equal(WebData_missing_V_G_sBar, 0.00207, tolerance = .0001, scale = 1)
  expect_equal(WebData_missing_V_G_uBarLCL, 0.0047, tolerance = .0001, scale = 1)
})



WebData_missing_formula_sBarLCL <- sBar_LCL(web_data_missing, formula = values~groups) # 0
WebData_missing_formula_sBar <- sBar(web_data_missing, formula = values~groups) # .00207
WebData_missing_formula_sBarUCL <- sBar_UCL(web_data_missing, formula = values~groups) # .00472
testthat::test_that("sBar sBar_UCL sBar_LCL missing work using formula", {
  expect_equal(WebData_missing_formula_sBarLCL, 0, tolerance = .01, scale = 1)
  expect_equal(WebData_missing_formula_sBar, 0.00207, tolerance = .0001, scale = 1)
  expect_equal(WebData_missing_formula_sBarUCL, 0.0047, tolerance = .0001, scale = 1)
})

QC_Lines(web_data, value = "values", grouping = "groups", method = "sBar") #
QC_Lines(web_data, formula = values~groups, method = "sBar") #Same As Above
QC_Lines(web_data_missing, value = "values", grouping = "groups", method = "sBar") #Same As Above

WebData_missing_formula_DF <- as.data.frame.list(QC_Lines(web_data_missing, formula = values~groups, method = "sBar")) #Same As Above
sBar_n_innert <-  as.data.frame.list(QC_Lines(web_data_missing, formula = values~groups, n=10, method = "sBar"))
testthat::test_that("missing value sBar sBar_UCL sBar_LCL n inert", {
  expect_equal(WebData_missing_formula_DF, sBar_n_innert, tolerance = .0001, scale = 1)
})

#Does not repond to n
#QC_Lines(web_data_missing, formula = values~groups, n=30, method = "rBar")
set.seed(5555)
nTest <- data.frame(v=rnorm(26, 0, 1), g=rep(letters[1:26], each=1))
testthat::test_that("warnings tripped for n = 1", {
  testthat::expect_warning(sBar_UCL(data = nTest, formula = v~g))
  testthat::expect_warning(QC_Lines(data = nTest, formula = v~g, n=20, method="sBar"))
})


WebData_V_G_DF <- as.data.frame.list(QC_Lines(web_data, value = "values", grouping = "groups", method = "sBar")) #
WebData_formual_DF <- as.data.frame.list(QC_Lines(web_data, formula = values~groups, method = "sBar")) #Same As Above
WebData_missing_V_G_DF <- as.data.frame.list(QC_Lines(web_data_missing, value = "values", grouping = "groups", method = "sBar")) #Same As Above
WebData_missing_formual_DF <- as.data.frame.list(QC_Lines(web_data_missing, formula = values~groups, n=10, method = "sBar"))

sBar_QC_ylines_test <- rbind(WebData_V_G_DF, WebData_formual_DF,
                             WebData_missing_V_G_DF, WebData_missing_formual_DF)
# write.csv(x = sBar_QC_ylines_test, file = "tests/testthat/sbar_QC_ylines_results.csv", quote = F, row.names = F)
sBar_QC_ylines_results <- read.csv(file = "sbar_QC_ylines_results.csv", header=TRUE)

testthat::test_that("QC_ylines method = sBar works", {
  expect_equal(sBar_QC_ylines_test, sBar_QC_ylines_results, tolerance = .0001, scale = 1)
})





# Short Test of Xbar_sBar -------------------------------------------------

xBar_sBar_QC_ylines_test <- as.data.frame.list(QC_Lines(web_data, formula = values~groups, method = "xBar.sBar"))
#write.csv(x = xBar_sBar_QC_ylines_test, file = "tests/testthat/sxBar_sBar_QC_ylines_results.csv", quote = F, row.names = F)
sBar_QC_ylines_results <- read.csv(file = "sxBar_sBar_QC_ylines_results.csv", header=TRUE)

context("xBar_sBar_LCL -- xBar_sBar_UCL")
testthat::test_that("QC_ylines method = sBar works", {
  expect_equal(xBar_sBar_LCL(web_data, formula = values~groups), 0.01272201, tolerance = .01, scale = 1)
  expect_equal(xBar_sBar_UCL(web_data, formula = values~groups), 0.01855799, tolerance = .01, scale = 1)
  expect_equal(xBar_sBar_QC_ylines_test, sBar_QC_ylines_results, tolerance = .01, scale = 1)
})

# XBAR
  # The difference between me and that data in the expected web data
  # is due to rounding.
  # 0.015640000-0.01272201 = 0.00291799
  # 0.015640000-0.01855799 = 0.00291799
  # .016 - 0.00291799 = 0.01308201
  # .016 + 0.00291799 = 0.01891799
  # UCL = 0.016 + [(1.427)(0.002)] = 0.0189 (3a)
  # LCL = 0.016 − [(1.427)(0.002)] = 0.0131
# SBAR
# .002078*2.089
# UCL = ( 2.089 )(0.002) = 0.00418 (1a) B4 is right
# LCL = (0.030)(0.002) = 0.00006 (2a) (wrong) B3 not right
  # LCL = (0.000)(0.002) = 0.00000 (2a) (correct) B3


# X Bar And S Charts Mini Tutorial
# https://www.slideshare.net/ahmad1957/x-bar-and-s-charts-mini-tutorial-presentation
# 1. Tools of Statistical Process Control: x and s Charts By James A. Patterson Operations Management 380 S. Thomas Foster, Ph. D. October 14, 2004
# 2. Tools of Statistical Process Control: x and s Charts Control Charts for Statistical Process Control The development and use of statistical tools known as control charts for monitoring and managing a wide variety of processes has historically been credited to Dr. Walter Shewart. During the 1920s, Dr. Shewart identified two types of variation that prevailed among all processes: chance cause variation and assignable cause variation (Swift, pp 135). Any process will inherently contain chance cause variation, as this is normal, random variation present in all systems. However, a process that contains anything other than chance cause or random variation is exhibiting assignable cause variation and is referred to as “out of statistical control.” Control charts may be used to monitor a process to determine whether or not it is in statistical control, to evaluate a process and determine normal statistical control parameters, and to identify areas of improvement in processes (Swift, pp 135 - 136). Control charts may be used to monitor variables that are continuous in nature, or non-continuous attributes. Continuous variables include quantifiable or measurable values that can be calculated across a continuous range, such as averages, dimensions (length, height, width), weight, and temperature. Attributes are not inherently quantifiable, but can be counted, such as number of parts defected, or number of defects per unit inspected (Smith, pp 158 -159). The control chart discussed in this paper is the union of both the x (or x-bar) and s charts, and is used to monitor processes with continuous variables. Rationale for Statistical Process Control Charts The statistical argument for control charts relies upon the Central Limit Theorem. The central limit theorem suggests that even if the underlying population from which a series of observations are gathered is not normally distributed, the resulting distribution of averages from the sample observations will be normally distributed around an average value, and that 99.73 1
# 3. Tools of Statistical Process Control: x and s Charts percent of the values will be contained within three standard deviations of that average value. Using this theorem, a control chart can be constructed that utilizes the statistical mean as the reference value or centerline on the chart, and an upper and lower control limit equivalent to ±3σ (three standard deviations) from the statistical mean. If an observed value falls beyond an upper or lower control limit, it can be concluded that this value is in an extreme tail of the distribution (only 0.27 percent of values should fall in this region), and is therefore out of control. Additionally, a series of values above or below the reference line can be evaluated to determine process stability. This will be discussed in more detail at a later point. Uses of the x and s Charts The x and s charts are typically implemented together, as the two charts offer different, but closely related forms of information about a process. The x chart relays information about the central tendency of the data, or the tendency of the measurements to accumulate in a normal distribution around an average value, or x (x-bar). While this is an important statistic to know and understand, it is also critical that the amount of variation around a particular average is also known. The s chart, recording the standard deviation from the average value in a given sample, is used for measuring and monitoring variation, and together x and s charts can provide insight into the stability of a process or system. For example, a sample of five measurements might have an average value ( x ) of 100 with a standard deviation (s) of 10; while another sample of five measurements might have an average value ( x ) of 100 with a standard deviation (s) of 30. Although the average values are the same in both samples, the sample with s equal to 30 exhibits much more variation. In some processes, variation within broad control limits might be acceptable, but in others, this kind of variation might be undesirable. In some applications, such is in high-tech manufacturing or machine part manufacturing, tolerances for variation are often 2
# 4. Tools of Statistical Process Control: x and s Charts very limited. The use of the x and s charts can be valuable in monitoring these types of processes. Gathering a Sample and Preparing the Data Frequently, due to the expensive nature of processes (capital or labor) in which the x and s charts are utilized, only small sample sizes are available. As with most statistical analysis, larger sample sizes are preferred, but in this case, not absolutely required. For purposes of simplification and demonstration, a sample size, n, equal to five and the number of samples taken, k, equal to ten, will be used. In other words, in each of ten samples (k = 10), there will be five observations (n = 5). The observations are recorded, and average values (x-bar) and standard deviations are calculated for each sample. Additionally, average s and x-bar values are calculated for the all of the sample averages and standard deviations. The resulting measurements and calculations can be recorded in a grid, such as in Table 1 (Page 3). Samplek 1 2 3 4 5 6 7 8 9 10 1 0.015 0.018 0.017 0.018 0.014 0.017 0.012 0.014 0.015 0.015 Observationn 2 0.019 0.017 0.018 0.016 0.013 0.015 0.013 0.015 0.018 0.015 3 0.022 0.013 0.019 0.012 0.014 0.018 0.015 0.014 0.016 0.018 4 0.016 0.014 0.014 0.020 0.015 0.019 0.015 0.013 0.014 0.016 5 0.013 0.015 0.015 0.019 0.017 0.014 0.016 0.012 0.013 0.017 x-bar 0.017 0.015 0.017 0.017 0.015 0.017 0.014 0.014 0.015 0.016 s 0.004 0.002 0.002 0.003 0.002 0.002 0.002 0.001 0.002 0.001 Average x-bar 0.016 Average s 0.002 Table 1: Completed grid with measurements and calculations The calculation of x and s in each sample can be accomplished by using formulas in a spreadsheet package, such as Microsoft ExcelTM [ x : =AVERAGE(cell range:cell range), and 3
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            5. Tools of Statistical Process Control: x and s Charts s: =SDEVA(cell range:cell range)] or by utilizing common statistical equations for these values (not discussed here). Calculating Upper and Lower Control Limits The calculated average x-bar and average s values are used as the centerlines, or reference values on each chart, against which the average values from each sample are plotted. The last step in preparation for configuring the control charts is the calculation of the upper and lower control limits for both the x and s charts. The first set of control limits calculated is the set for the s chart. The reason these are calculated in advance of the x control limits is twofold: first, to determine if the process is in statistical control with regard to variance; and second, to provide insight as to why the process is out of control. Once the process has been brought under control with regard to variance, the x chart can be constructed (Smith, pp 237). The formulas for calculating the control limits for each chart are derived through a lengthy statistical proof that is based, as previously referenced, on the application of the Central Limit Theorem. The end result of the proof demonstrates that calculated statistical constants, based on the number of observations in each sample, may be substituted for more complicated formulas when determining the Upper Control Limit (UCL) and Lower Control Limit (LCL). The statistical proof and equations are not discussed here, as this documentation emphasizes the applied techniques, rather than the details of the theory. The n B3 B4 C4 A3 constants are reproduced in Table 2 (Foster, pp .365), 2 0.000 3.267 0.7979 2.659 3 0.000 2.568 0.8862 1.954 through n values of six. Using the constants in Table 4 0.000 2.666 0.9213 1.628 5 0.030 2.089 0.9400 1.427 2, the formulas for calculating the UCL and LCL for 6 0.118 1.970 0.9515 1.287 the s chart are described below: Table 2: x-bar and s chart values 4
# 6. Tools of Statistical Process Control: x and s Charts UCL = B4 ( s ) Calculate the Upper Control Limit for the s chart (1) LCL = B3 ( s ) Calculate the Lower Control Limit for the s chart (2) Using the values calculated in Table 1, and the constants from Table 2, the following s chart control limits can be calculated: UCL = ( 2.089 )(0.002) = 0.00418 (1a) LCL = (0.030)(0.002) = 0.00006 (2a) As previously stated, after the control values for the s chart are calculated, the control values for the x chart are determined, using the following equations: UCLx = x + A3 ( s ) (3) UCLx = x − A3 ( s ) (4) Again, using the values calculated in Table 1, and the constants from Table 2, the following x chart control limits can be calculated: UCL = 0.016 + [(1.427)(0.002)] = 0.0189 (3a) LCL = 0.016 − [(1.427)(0.002)] = 0.0131 (4a) With the centerline values, upper control limits, and lower control limits all calculated, the x and s charts can be constructed, and sample values plotted. 5
# 7. Tools of Statistical Process Control: x and s Charts Constructing the x and s charts Typically, the construction of the x and s charts is done together, so that the plotted values can be compared at even intervals. Below, the two charts are illustrated in Figures 1 and 2: x-bar Chart 0.020 0.019 0.018 0.017 0.016 x-bar 0.015 0.014 0.013 0.012 0.011 0.010 1 2 3 4 5 6 7 8 9 10 average x-bar 0.017 0.015 0.017 0.017 0.015 0.017 0.014 0.014 0.015 0.016 UCL 0.0189 0.0189 0.0189 0.0189 0.0189 0.0189 0.0189 0.0189 0.0189 0.0189 x-bar 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 LCL 0.0131 0.0131 0.0131 0.0131 0.0131 0.0131 0.0131 0.0131 0.0131 0.0131 Sample Figure 1: x-bar Chart s-bar Chart 0.00450 0.00400 0.00350 0.00300 0.00250 s-bar 0.00200 0.00150 0.00100 0.00050 0.00000 1 2 3 4 5 6 7 8 9 10 average s-bar 0.0035 0.0020 0.0020 0.0031 0.0015 0.0020 0.0016 0.0011 0.0019 0.0013 UCL 0.0041 0.0041 0.0041 0.0041 0.0041 0.0041 0.0041 0.0041 0.0041 0.0041 x-bar 0.0020 0.0020 0.0020 0.0020 0.0020 0.0020 0.0020 0.0020 0.0020 0.0020 LCL 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 Samples Figure 2: s-bar Chart 6
# 8. Tools of Statistical Process Control: x and s Charts Assessing the Results With respect to control charts, recording the observations, calculating the control values, and plotting the observed values on the x and s charts is only the beginning of the process. Once the chart is constructed, and data points are plotted, the chart needs to be evaluated to determine whether or not the process is operating in statistical control. There are several guidelines for evaluating control charts, all with a prevailing philosophy: the plotted points should only exhibit random variation that cannot be fitted with an identifiable or quantifiable pattern. Additionally, the following features, in accordance with the parameters of a normal distribution and the Central Limit Theorem, should also apply (Swift, pp 160): 1. 68 percent of the points should be within ±1σ of the reference line. 2. 4.27 percent of the points should be between ±2σ and ±3σ of the reference line. 3. No more than 0.27 percent of the points should exceed ±3σ of the reference line. Upper Control Limit In addition to the above general philosophy of Zone A evaluating the control chart, an additional set of Zone B Zone C Center Line guidelines, described in Swift (pp 161-163) are Zone C Zone B adapted from the AT&T Rules, as described in the Zone A Lower Control Limit AT&T Statistical Quality Control Handbook. The Figure 3: AT&T Zone Chart AT&T Rules divide the control chart into three zones, mirrored across the center line. This is illustrated in Figure 3.According to these guidelines, a process can be ruled “out of statistical control” if any of the following apply: 1. Any point falls outside of the upper or lower control limit ( beyond ±3σ). 2. Two of three successive points in zone A or beyond.. 3. Four out of five successive points on zone B or beyond. 4. Eight successive points fall in zone C or beyond, on one side of the center line. (list continues on page 8) 7
# 9. Tools of Statistical Process Control: x and s Charts 5. Trends: a series of points without any appreciable or random change in direction, or values moving continuously up and down, or across the centerline in pattern. 6. Cycles: short trends in which the data may repeat in a pattern. 7. Shifts: a sudden change in level in one direction or another. 8. Stratification: a pattern of “unnatural consistency” within a single zone, or near the centerline. 9. Systematic Variables: a predictable pattern, where a high point is always followed by a low point, or a low point by a high point. Evaluating the Example Case As the rules and guidelines are applied to the example provided in this document, it is apparent that the s chart is exhibiting signs of instability, or that the underlying process is out of statistical control. No values ever exceed control limits, however, there appears to be a sustained trend in variation from the positive side of the centerline to the negative or lower side of the control line. When checked against the x chart, it is less apparent that the system is out of control. The x and s chart together suggest that while the process output is consistently clustered around the mean value, the variation is out of control and needs to be addressed. Finding More Information About x and s Charts Most basic statistics or statistical process control reference books or textbooks contain a section relating to control charts and how to implement them. The internet can also provide an excellent resource for obtaining information about statistical process control charts. Due to the rapidly changing nature of the internet, a website is not listed here, however, a search on a popular search site for “x-bar and s charts” yielded several thousand possible references. Additionally, in the attached reference section are several textbooks that were valuable in compiling this document. 8
# 10. Tools of Statistical Process Control: x and s Charts References AT&T Statistical Quality Control Handbook. Charlotte, NC: Delmar, 1985 Foster, S. Thomas (2004). Managing Quality: An Integrative Approach, 2nd Edition. Upper Saddle River, NJ: Prentice-Hall Groebner, David F., Shannon, Patrick W., Fry, Phillip C., Smith, Kent D. (2000). Business Statistics: A Decision Making Approach. Upper Saddle River, NJ: Prentice-Hall Montgomer, Douglas C. (1991). Introduction to Statistical Quality Control, 2nd Edition. New York, NY: John Wiley & Sons Smith, Gerald M. (1998). Statistical Process Control and Quality Improvement, 3rd Edition. Upper Saddle River, NJ: Prentice-Hall Swift, J.A. (1995). Introduction to Modern Statistical Quality Control and Management. Delray Beach, FL: St. Lucie Press 9


# n <- 14
# set.seed(5555)
# nTest <- data.frame(v=rnorm(n*26, 0, 1), g=rep(letters[1:26], each=n))
# xBar_Bar(data = nTest, formula = v~g) + QC_Lines(data = nTest, formula = v~g, method="xBar.rBar")[2]*.235
# xBar_Bar(data = nTest, formula = v~g) + QC_Lines(data = nTest, formula = v~g, method="xBar.sBar")[2]*.817
#
# QC_Lines(data = nTest, formula = v~g, method="xBar.rBar")[7]
# QC_Lines(data = nTest, formula = v~g, method="xBar.sBar")[7]
