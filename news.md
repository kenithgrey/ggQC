
ggQC Version 0.0.3
==================

New functions
-------------

-   **stat\_QC\_Capability()** : NEW ggplot stat function to add QC lines, labels, and capability summaries to histograms and density charts (convienience function)

-   **QC\_Capability()** : NEW function to calculated capability parameters given LSL, USL, and a data.frame containing the process data.

-   **capability.summary()** : NEW function to calculate capability parmeters if the LSL, USL, process mean and sigma from QC charting are already known.

-   **stat\_QC\_cap\_hlabels()**: NEW ggplot stat function to add horizontal QC capability lables to histograms and density charts

-   **stat\_QC\_cap\_vlabels()** : NEW ggplot stat function to add vertical QC capability lables to histograms and density charts

-   **stat\_QC\_cap\_hlines()** : NEW ggplot stat function to add horizontal QC capability lines to histograms and density charts

-   **stat\_QC\_cap\_vlines()** : NEW ggplot stat function to add vertical QC capability lines to histograms and density charts

-   **stat\_QC\_cap\_summary()** : NEW ggplot stat function to display QC capability parameters on a chart.

Updated Functions
-----------------

-   **QC\_Violations(show.facets=c(1:4))**: Use new parameter show.facets to display specific QC violation facet(s).

-   **stat\_QC** : New auto labeling capabilites, simplifide ggplot syntax for making mR and xBar style plots; can now display 1 and 2 sigma lines, and accomodate upper and lower physical limits.

ggQC Version 0.0.2
==================

New Functions
-------------

-   **QC\_Violations()**: NEW function that calculates QC violations for Shewart Quality Control Charts.

-   **stat\_qc\_violations()**: NEW ggplot stat function to graphically view QC violations in a data set.

Updated Functions
-----------------

-   **QC\_Lines()**: Updated function output to include the calculated sigma value.

Resolved Issues
---------------

-   Fixed (\#32): Observed that when n &gt; 100 I got a convergence error for D5. Unfortunately this causes problems with all the quailty control charts if n &gt; 100.

ggQC Version 0.0.1
==================

New Functions
-------------

-   **QC\_Lines()** : Output quality control chart data in table format
-   **stat\_QC()** : ggplot stat to generate control charts supported methods
-   **stat\_mR()**: ggplot stat to generate mR chart.
-   **stat\_QC\_labels()**: ggplot stat to write text labels on control chart center line and limits.
-   30 plus functions to calculate specific control chart constants. (recommend using QC\_Lines())
