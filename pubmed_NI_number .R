library(ggplot2)
# Pubmed was searched on 18-Oct-2018
#English[Language] AND (non-inferiority[Title/Abstract] OR noninferior[Title/Abstract] OR non-inferior[Title/Abstract] OR noninferiority[Title/Abstract]) AND ("2017/06/01"[PDAT] : "2018/05/31"[PDAT]) AND Clinical Trial[ptyp] + filter clinical trials

pub.n <- data.frame(year=c(2010, 2011, 2012, 2013, 2014, 2015, 2016), 
                    n=c(296, 358, 415, 487, 563, 624, 666))

g <- ggplot2::ggplot(pub.n, aes(x = year, y = n)) + 
  #ggplot2::geom_line(linetype = "dashed") +
  #ggplot2::geom_bar(stat='identity') +
  geom_segment(aes(x = year, y = 0, xend = year, yend = n)) +
  geom_point() +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Number of published papers") +
  #ggplot2::ggtitle("Number of published non-inferiroty papers by year (Pubmed)") +
  ggplot2::theme_bw() +
  ggplot2::scale_x_continuous(breaks = 2010:2016) +
  ggplot2::scale_y_continuous(breaks=seq(0,700,100))

pdf("Review paper/pubmed_review_ni.pdf")
g
dev.off()