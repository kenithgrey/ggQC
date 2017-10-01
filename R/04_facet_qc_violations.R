Facet_QC_VIOLATIONS <-
  ggplot2::ggproto("Facet_QC_VIOLATIONS", ggplot2::FacetWrap,
                    compute_layout = function(data, params, method=method) {

                ### Test Area ###
                #print("facet")
                # print(paste0("Method:", params$method))
                # print(params$plot_env)
                # print(str(data))
                #print(head(data))
                #print(data$y)
                #df <- data[[1]] # copy the data
                #print(head(df))
                #df$Index <- 1:nrow(df)
        # if (params$method == "XmR"){
        #   }else if(method %in% c("c", "p", "u", "np")){
        #     print("c, p, u, and np charts not supported by stat_qc_violations")
        #   }else{
        #     print("Do XbarR type anaylsis")
        # }

        id <- factor(unique(RuleSet$Alt_Description))

        layout <- data.frame(PANEL = factor(id,
                   levels = c("Violation Same Side",
                              "Violation 1 Sigma",
                              "Violation 2 Sigma",
                              "Violation 3 Sigma")))

        #Set the Row and Col, and order the
        layout$ROW <- 1
        layout$COL <- as.numeric(layout$PANEL)
        layout <- layout[order(layout$PANEL), , drop = FALSE]
        rownames(layout) <- NULL

        # Add scale identification
        layout$SCALE_X <- if (params$free$x) id else 1L
        layout$SCALE_Y <- if (params$free$y) id else 1L
        cbind(layout, .violations = layout$PANEL)
      },

      # if(method %in% method %in% c("xBar.rBar", "xBar.rMedian", "xBar.sBar", "xMedian.rBar", "xMedian.rMedian"){
      #
      #
      # }


      map_data = function(data, layout, params) {
        if (is.null(data) || nrow(data) == 0) {
            return(cbind(data, PANEL = integer(0)))
        }

        data <- do.call(rbind,
                        lapply(layout$PANEL,
                               function(x){
                                    data$PANEL <- x
                                    data$group <- 1
                                    return(data)})

                )

      }
)
#' @export
facet_qc_violations <-
  function(nrow = NULL, ncol = NULL,
           scales = "fixed", shrink = TRUE,
           strip.position = "top", method=method){

    facet <- ggplot2::facet_wrap(
                      ~.violations, nrow = nrow,
                      ncol = ncol,
                      scales = scales,
                      shrink = shrink,
                      strip.position = strip.position)

    facet$params$method <- method

    ggproto(NULL, Facet_QC_VIOLATIONS,
          shrink = shrink, params = facet$params
  )
}
