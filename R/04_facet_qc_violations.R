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
                if (params$method == "XmR"){
                  #print("Do XmR")
                  #viloation_df <- QC_Violations(data = data$y, method = "XmR")
                  #df2 <- merge(df, viloation_df, by="Index", all.x = TRUE)
                  #df2$PANEL <- as.numeric(factor(df2$Violation_Result))

                  #print(head(df2))
                }else if(method %in% c("c", "p", "u", "np")){
                  print("c, p, u, and np charts not supported by Stat_qc_violations")
                }else{
                  print("Do XbarR type anaylsis")
                }

                id <- factor(unique(RuleSet$Alt_Description))
                #dims <- wrap_dims(params$n, params$nrow, params$ncol)
                #dims <- wrap_dims(4L, 1L, 4L)

                layout <- data.frame(
                  PANEL = factor(id,
                           levels = c("Violation Same Side",
                                      "Violation 1 Sigma",
                                      "Violation 2 Sigma",
                                      "Violation 3 Sigma")))

                if (params$as.table) {
                  layout$ROW <- 1
                } else {
                  layout$ROW <- 1
                }
                layout$COL <- as.numeric(layout$PANEL)

                layout <- layout[order(layout$PANEL), , drop = FALSE]
                rownames(layout) <- NULL

                # Add scale identification
                layout$SCALE_X <- if (params$free$x) id else 1L
                layout$SCALE_Y <- if (params$free$y) id else 1L
                #print(layout)
                cbind(layout, .violations = layout$PANEL)
              },

              map_data = function(data, layout, params) {
                if (is.null(data) || nrow(data) == 0) {
                  #print(data)
                  return(cbind(data, PANEL = integer(0)))
                }
                #print(head(data))
                #print(head(data, 33))
                data <- do.call(rbind, lapply(layout$PANEL, function(x){
                                            data$PANEL <- x
                                            data$group <- 1
                                            return(data)
                                                      })

                  )
                #df2
                #n_samples <- round(nrow(data) * params$prop)
                # new_data <- lapply(seq_len(params$n), function(i) {
                #   cbind(data[sample(nrow(data), n_samples), , drop = FALSE], PANEL = i)
                # })
                # do.call(rbind, new_data)
              #print(data)
              #data
              }
)
#' @export
facet_qc_violations <- function(nrow = NULL, ncol = NULL,
                            scales = "fixed", shrink = TRUE,
                            strip.position = "top", method=method
                            ) {

  facet <- ggplot2::facet_wrap(~.violations, nrow = nrow,
                               ncol = ncol,
                      scales = scales,
                      shrink = shrink,
                      strip.position = strip.position)
  #facet$params$n <- n
  facet$params$method <- method
  ggproto(NULL, Facet_QC_VIOLATIONS,
          shrink = shrink, params = facet$params
  )
}
