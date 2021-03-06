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
  ggplot2::scale_y_continuous(breaks=seq(0,700,100))+
  ggplot2::theme(text = element_text(size = 20)
                 #strip.text = element_text(size = rel(0.7))
                 )

pdf("pubmed_review_ni.pdf")
g
dev.off()

#Based on the referee comments, the search was done again to include years 2017 and 2018
library(ggplot2)
# Pubmed was searched on 24-Jan-2019
#English[Language] AND (non-inferiority[Title/Abstract] OR noninferior[Title/Abstract] OR non-inferior[Title/Abstract] OR noninferiority[Title/Abstract]) AND ("2017/06/01"[PDAT] : "2018/05/31"[PDAT]) AND Clinical Trial[ptyp] + filter clinical trials

pub.n <- data.frame(year=c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), 
                    n=c(296, 358, 415, 487, 563, 624, 670, 666))

g <- ggplot2::ggplot(pub.n, aes(x = year, y = n)) + 
  #ggplot2::geom_line(linetype = "dashed") +
  #ggplot2::geom_bar(stat='identity') +
  geom_segment(aes(x = year, y = 0, xend = year, yend = n)) +
  geom_point() +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Number of published papers") +
  #ggplot2::ggtitle("Number of published non-inferiroty papers by year (Pubmed)") +
  ggplot2::theme_bw() +
  ggplot2::scale_x_continuous(breaks = 2010:2017) +
  ggplot2::scale_y_continuous(breaks=seq(0,700,100))+
  ggplot2::theme(text = element_text(size = 20)
                 #strip.text = element_text(size = rel(0.7))
  )

pdf("pubmed_review_ni1.pdf")
g
dev.off()


#Poster Prep
library(ggplot2)
# Pubmed was searched on 28-Apr-2019
#English[Language] AND (non-inferiority[Title/Abstract] OR noninferior[Title/Abstract] OR non-inferior[Title/Abstract] OR noninferiority[Title/Abstract]) AND ("2017/06/01"[PDAT] : "2018/05/31"[PDAT]) AND Clinical Trial[ptyp] + filter clinical trials

pub.n <- data.frame(year=c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), 
                    n=c(296, 358, 415, 487, 563, 625, 672, 687))

g <- ggplot2::ggplot(pub.n, aes(x = year, y = n)) + 
  #ggplot2::geom_line(linetype = "dashed") +
  #ggplot2::geom_bar(stat='identity') +
  geom_segment(aes(x = year, y = 0, xend = year, yend = n), size=1) +
  geom_point(size=5) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Number of published papers") +
  #ggplot2::ggtitle("Number of published non-inferiroty papers by year (Pubmed)") +
  ggplot2::theme_bw() +
  ggplot2::scale_x_continuous(breaks = 2010:2017) +
  ggplot2::scale_y_continuous(breaks=seq(0,700,100))+
  ggplot2::theme(text = element_text(size = 20)
                 #strip.text = element_text(size = rel(0.7))
  )

pdf("pubmed_review_ni_apr2019.pdf")
g
dev.off()



#APHA Beamer Prep
library(ggplot2)
# Pubmed was searched on 11-Oct-2019 to update 2017
#English[Language] AND (non-inferiority[Title/Abstract] OR noninferior[Title/Abstract] OR non-inferior[Title/Abstract] OR noninferiority[Title/Abstract]) AND ("2017/06/01"[PDAT] : "2018/05/31"[PDAT]) AND Clinical Trial[ptyp] + filter clinical trials

pub.n <- data.frame(year=c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), 
                    n=c(296, 358, 415, 487, 563, 625, 672, 713))

g <- ggplot2::ggplot(pub.n, aes(x = year, y = n)) + 
  #ggplot2::geom_line(linetype = "dashed") +
  #ggplot2::geom_bar(stat='identity') +
  geom_segment(aes(x = year, y = 0, xend = year, yend = n), size=1) +
  geom_point(size=5) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Number of published papers") +
  #ggplot2::ggtitle("Number of published non-inferiroty papers by year (Pubmed)") +
  ggplot2::theme_bw() +
  ggplot2::scale_x_continuous(breaks = 2010:2017) +
  ggplot2::scale_y_continuous(breaks=seq(0,700,100))+
  ggplot2::theme(text = element_text(size = 20)
                 #strip.text = element_text(size = rel(0.7))
  )

pdf("pubmed_review_ni_oct2019.pdf")
g
dev.off()


#SBR review update
library(ggplot2)
# Pubmed was searched on 27-Apr-2020 to update 2019
#English[Language] AND (non-inferiority[Title/Abstract] OR noninferior[Title/Abstract] OR non-inferior[Title/Abstract] OR noninferiority[Title/Abstract]) AND ("2017/06/01"[PDAT] : "2018/05/31"[PDAT]) AND Clinical Trial[ptyp] + filter clinical trials

pub.n <- data.frame(year=c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019), 
                    n=c(296, 358, 415, 487, 563, 625, 672, 722, 714, 441))

g <- ggplot2::ggplot(pub.n %>% filter(year != 2019), aes(x = year, y = n)) + 
  #ggplot2::geom_line(linetype = "dashed") +
  #ggplot2::geom_bar(stat='identity') +
  geom_segment(aes(x = year, y = 0, xend = year, yend = n), size=1) +
  geom_point(size=5) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Number of published papers") +
  #ggplot2::ggtitle("Number of published non-inferiroty papers by year (Pubmed)") +
  ggplot2::theme_bw() +
  ggplot2::scale_x_continuous(breaks = 2010:2018) +
  ggplot2::scale_y_continuous(breaks=seq(0,700,100))+
  ggplot2::theme(text = element_text(size = 20)
                 #strip.text = element_text(size = rel(0.7))
  )

pdf("pubmed_review_ni_apr2020.pdf")
g
dev.off()
