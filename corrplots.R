source("/Users/oshi/Desktop/TB/impute.R") #change to source of your imputation script

library(cowplot)

# function body
ggcorrplot2 <- function(corr,
                       method = c("square", "circle"),
                       type = c("full", "lower", "upper"),
                       ggtheme = ggplot2::theme_minimal,
                       title = "",
                       show.legend = TRUE,
                       legend.title = "Corr",
                       show.diag = NULL,
                       colors = c("blue", "white", "red"),
                       outline.color = "gray",
                       hc.order = FALSE,
                       hc.method = "complete",
                       lab = FALSE,
                       lab_col = "black",
                       lab_size = 4,
                       p.mat = NULL,
                       sig.level = 0.05,
                       insig = c("pch", "blank"),
                       pch = 4,
                       pch.col = "black",
                       pch.cex = 5,
                       tl.cex = 12,
                       tl.col = "black",
                       tl.srt = 45,
                       digits = 2,
                       as.is = FALSE) {
  type <- match.arg(type)
  method <- match.arg(method)
  insig <- match.arg(insig)
  if(is.null(show.diag)){
    if(type == "full") show.diag <- TRUE
    else show.diag <- FALSE
  }
  
  if(inherits(corr, "cor_mat")){
    # cor_mat object from rstatix
    cor.mat <- corr
    corr <- .tibble_to_matrix(cor.mat)
    p.mat <- .tibble_to_matrix(attr(cor.mat, "pvalue"))
  }
  
  if (!is.matrix(corr) & !is.data.frame(corr)) {
    stop("Need a matrix or data frame!")
  }
  corr <- as.matrix(corr)
  
  corr <- base::round(x = corr, digits = digits)
  
  if (hc.order) {
    ord <- .hc_cormat_order(corr, hc.method = hc.method)
    corr <- corr[ord, ord]
    if (!is.null(p.mat)) {
      p.mat <- p.mat[ord, ord]
      p.mat <- base::round(x = p.mat, digits = digits)
    }
  }
  
  if(!show.diag){
    corr <- .remove_diag(corr)
    p.mat <- .remove_diag(p.mat)
  }
  
  # Get lower or upper triangle
  if (type == "lower") {
    corr <- .get_lower_tri(corr, show.diag)
    p.mat <- .get_lower_tri(p.mat, show.diag)
  }
  else if (type == "upper") {
    corr <- .get_upper_tri(corr, show.diag)
    p.mat <- .get_upper_tri(p.mat, show.diag)
  }
  
  # Melt corr and pmat
  corr <- reshape2::melt(corr, na.rm = TRUE, as.is = as.is)
  colnames(corr) <- c("Var1", "Var2", "value")
  corr$pvalue <- rep(NA, nrow(corr))
  corr$signif <- rep(NA, nrow(corr))
  
  if (!is.null(p.mat)) {
    p.mat <- reshape2::melt(p.mat, na.rm = TRUE)
    corr$coef <- corr$value
    corr$pvalue <- p.mat$value
    corr$signif <- as.numeric(p.mat$value <= sig.level)
    p.mat <- subset(p.mat, p.mat$value <= sig.level)
    if (insig == "blank") {
      corr$value <- corr$value * corr$signif
    }
  }
  
  
  corr$abs_corr <- abs(corr$value) * 10
  
  # heatmap
  p <-
    ggplot2::ggplot(
      data = corr,
      mapping = ggplot2::aes_string(x = "Var1", y = "Var2", fill = "value")
    )
  
  # modification based on method
  if (method == "square") {
    p <- p +
      ggplot2::geom_tile(color = outline.color)
  } else if (method == "circle") {
    p <- p +
      ggplot2::geom_point(
        color = outline.color,
        shape = 21,
        ggplot2::aes_string(size = "abs_corr")
      ) +
      ggplot2::scale_size(range = c(4, 10)) +
      ggplot2::guides(size = FALSE)
  }
  
  # adding colors
  p <-
    p + ggplot2::scale_fill_gradient2(
      low = colors[1],
      high = colors[3],
      mid = colors[2],
      midpoint = 0,
      limit = c(-1, 1),
      space = "Lab",
      name = legend.title
    )
  
  # depending on the class of the object, add the specified theme
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme()
  } else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }
  
  
  p <- p +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = tl.srt,
        vjust = 1,
        size = tl.cex,
        hjust = 1
      ),
      axis.text.y = ggplot2::element_text(size = tl.cex)
    ) +
    ggplot2::coord_fixed()
  
  label <- round(x = corr[, "value"], digits = digits)
  if(!is.null(p.mat) & insig == "blank"){
    ns <- corr$pvalue > sig.level
    if(sum(ns) > 0) label[ns] <- " "
  }
  
  # matrix cell labels
  if (lab) {
    p <- p +
      ggplot2::geom_text(
        mapping = ggplot2::aes_string(x = "Var1", y = "Var2"),
        label = label,
        color = lab_col,
        size = lab_size
      )
  }
  
  # matrix cell glyphs
  if (!is.null(p.mat) & insig == "pch") {
    p <- p + ggplot2::geom_point(
      data = p.mat,
      mapping = ggplot2::aes_string(x = "Var1", y = "Var2"),
      shape = 8,
      size = 1,
      color = pch.col,
      position = position_nudge(x=0.35,y=0.16)
    )
  }
  # pch = 8, pch.cex =1
  # add titles
  if (title != "") {
    p <- p +
      ggplot2::ggtitle(title)
  }
  
  # removing legend
  if (!show.legend) {
    p <- p +
      ggplot2::theme(legend.position = "none")
  }
  
  # removing panel
  p <- p +
    .no_panel()
  p
}



#' Compute the matrix of correlation p-values
#'
#' @param x numeric matrix or data frame
#' @param ... other arguments to be passed to the function cor.test.
#' @rdname ggcorrplot
#' @export

cor_pmat <- function(x, ...) {
  
  # initializing values
  mat <- as.matrix(x)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  
  # creating the p-value matrix
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- stats::cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  
  # name rows and columns of the p-value matrix
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  
  # return the final matrix
  p.mat
}



#+++++++++++++++++++++++
# Helper Functions
#+++++++++++++++++++++++

# Get lower triangle of the correlation matrix
.get_lower_tri <- function(cormat, show.diag = FALSE) {
  if (is.null(cormat)) {
    return(cormat)
  }
  cormat[upper.tri(cormat)] <- NA
  if (!show.diag) {
    diag(cormat) <- NA
  }
  return(cormat)
}

# Get upper triangle of the correlation matrix
.get_upper_tri <- function(cormat, show.diag = FALSE) {
  if (is.null(cormat)) {
    return(cormat)
  }
  cormat[lower.tri(cormat)] <- NA
  if (!show.diag) {
    diag(cormat) <- NA
  }
  return(cormat)
}

.remove_diag <- function(cormat){
  if (is.null(cormat)) {
    return(cormat)
  }
  diag(cormat) <- NA
  cormat
}
# hc.order correlation matrix
.hc_cormat_order <- function(cormat, hc.method = "complete") {
  dd <- stats::as.dist((1 - cormat) / 2)
  hc <- stats::hclust(dd, method = hc.method)
  hc$order
}

.no_panel <- function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank()
  )
}


# Convert a tbl to matrix
.tibble_to_matrix <- function(x){
  x <-  as.data.frame(x)
  rownames(x) <- x[, 1]
  x <- x[, -1]
  as.matrix(x)
}

######################
#Full dataset corrplot
######################

#recode clinic data into numeric to be used for corrplot
#clinic.corr.recoded =  imputed.dataset.1.filtered.1 %>% mutate(Clinic_Type = case_when(Clinic_Type == "Rural"  ~    0, Clinic_Type == "Small town" ~ 1, Clinic_Type == "Large town"  ~   2  , TRUE ~ NA_real_))

clinic.birthplace.corr.recoded =  imputed.dataset.1.filtered %>% mutate(birthplace_type = case_when(birthplace_type == "Rural"  ~    0, birthplace_type == "Town" ~ 1 , TRUE ~ NA_real_)) %>% mutate(Clinic_Type = case_when(Clinic_Type == "Rural"  ~    0, Clinic_Type == "Small town" ~ 1, Clinic_Type == "Large town"  ~   2  , TRUE ~ NA_real_))  %>% mutate(migration = case_when(birthplace == "Rural-Rural"  ~    0, birthplace == "Rural-Rural" ~ 1, birthplace == "Large town"  ~   2  , TRUE ~ NA_real_))



#select vars for correlation
imp.data.select =clinic.corr.recoded %>% select(sex,age,smokes,Years.of.education,diabetes, drnk_alcohol, Clinic_Type)%>% mutate(across(c(smokes,sex,drnk_alcohol,diabetes,Clinic_Type), as.character)) %>% mutate(across(c(smokes,sex,drnk_alcohol,diabetes,Clinic_Type), as.numeric))

#compute p values
imp.corrplot.pmat = cor_pmat(imp.data.select)

imp.corrplot2 = model.matrix(~0+., data=imp.data.select) %>% 
  cor(use="everything") %>% 
  ggcorrplot2(show.diag = F, type="lower", lab=TRUE, lab_size=4, title = "",hc.order = FALSE,legend.title = "Pearson\nCorrelation\n",p.mat = imp.corrplot.pmat, sig.level = 0.05 ) +scale_x_discrete(breaks = c("sex","age","smokes","Years.of.education" ,"diabetes","drnk_alcohol","Clinic_Type"),labels = c( "Sex","Age","Smoker","Years of Education","Diabetes","Drinks Alcohol","Town Size")) +scale_y_discrete(breaks = c("sex","age","smokes","Years.of.education" ,"diabetes","drnk_alcohol","Clinic_Type"),labels = c( "Sex","Age","Smoker","Years of Education","Diabetes","Drinks Alcohol","Town Size"))


#####################

#alternate education corrplot
###############
#recode education2 data into numeric to be used for corrplot
educ.corr.recoded =  clinic.corr.recoded %>% mutate(Education3 = case_when(Education2 == "None" ~ 0,Education2 == "Primary" ~ 1,Education2 ==  "Secondary" ~ 2, Education2 == "Tertiary"~ 3))  %>% relocate(Education3, .after = Education2)



#select vars for correlation and make sure all numeric
imp.data.select2 =educ.corr.recoded %>% select(sex,age,smokes,Education3,diabetes, drnk_alcohol, Clinic_Type)%>% mutate(across(c(smokes,sex,drnk_alcohol,diabetes,Clinic_Type), as.character)) %>% mutate(across(c(smokes,sex,drnk_alcohol,diabetes,Clinic_Type), as.numeric))

#compute p values
imp.corrplot.pmat2 = cor_pmat(imp.data.select2)

imp.corrplot.new.educ = model.matrix(~0+., data=imp.data.select2) %>% 
  cor(use="everything") %>% 
  ggcorrplot2(show.diag = F, type="lower", lab=TRUE, lab_size=4, title = "",hc.order = FALSE,legend.title = "Pearson\nCorrelation\n",p.mat = imp.corrplot.pmat2, sig.level = 0.05 ) +scale_x_discrete(breaks = c("sex","age","smokes","Education3" ,"diabetes","drnk_alcohol","Clinic_Type"),labels = c( "Sex","Age","Smoker","Education Level","Diabetes","Drinks Alcohol","Town Size")) +scale_y_discrete(breaks = c("sex","age","smokes","Education3" ,"diabetes","drnk_alcohol","Clinic_Type"),labels = c( "Sex","Age","Smoker","Education Level","Diabetes","Drinks Alcohol","Town Size"))

#############
## Khoesan corrplot
###############

#merge ancestry and epi csv files
#using clinic recoded which is already imputed and filtered

raw_merge<- merge(ancestry,clinic.corr.recoded, by ="record_id") #use for corrplot

KhoeSan.corr.data = raw_merge%>% select(sex,age,smokes,Years.of.education,diabetes,drnk_alcohol, Clinic_Type,KhoeSan)%>%mutate(across(c(smokes,sex,drnk_alcohol,diabetes,Clinic_Type), as.character)) %>% mutate(across(c(smokes,sex,drnk_alcohol,diabetes,Clinic_Type), as.numeric))

#compute p values
khoesan.corrplot.pmat = cor_pmat(KhoeSan.corr.data)

#khoesan corrplot
KhoeSan.corrplot = model.matrix(~0+., data=KhoeSan.corr.data) %>% 
  cor(use="everything") %>% 
  ggcorrplot2(show.diag = F, type="lower", lab=TRUE, lab_size=4, title = "",hc.order = FALSE,legend.title = "Pearson\nCorrelation\n", p.mat = khoesan.corrplot.pmat, sig.level = 0.05) +scale_x_discrete(breaks = c("sex","age","smokes","Years.of.education" ,"diabetes","drnk_alcohol","Clinic_Type","KhoeSan"),labels = c( "Sex","Age","Smoker","Years of Education","Diabetes","Drinks Alcohol","Town Size","KhoeSan Ancestry")) +scale_y_discrete(breaks = c("sex","age","smokes","Years.of.education" ,"diabetes","drnk_alcohol","Clinic_Type","KhoeSan"),labels = c( "Sex","Age","Smoker","Years of Education","Diabetes","Drinks Alcohol","Town Size","KhoeSan Ancestry"))

imp.corrplot2
KhoeSan.corrplot


plot_grid( imp.corrplot2, KhoeSan.corrplot, labels = "AUTO") 


 