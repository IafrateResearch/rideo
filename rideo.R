
## e.g:
## Rscript rideo.R GUIDE-Seq-Off-targets_simple.txt

infile = commandArgs(TRUE)[1]

cyto = read.table('cytoband.txt', header=T, stringsAsFactors=F)
head(cyto)
allsite = read.table(infile, sep='\t', stringsAsFactors=F, header=T)
head(allsite)

chrs = unique(cyto$Chromosome)
sites = unique(allsite$Targetsite)

## One pdf for each site
    for (site in sites){
	sitei = subset(allsite, Targetsite == site)

	pdf(paste('output/', site, '.pdf', sep=''))

	ymax = max(cyto$ChromEnd)
	plot(0, xlim=c(0,24), ylim=c(0,ymax), type='n'
		, axes=F, xlab='Chromosome', ylab=''
		, main=site)
	
	legend('topright', col=c(3,2), legend=c('on-target','off-target'), border=NA, pch='-')	
	xpos = 1

    for (chr in chrs) {
	di = cyto[cyto$Chromosome ==chr, ]
	ymax = max(di$ChromEnd)
        starts = ymax - di$ChromStart
        ends = ymax - di$ChromEnd
	x1 = rep(xpos-0.2, length(starts))
	x2 = rep(xpos+0.2, length(starts))
        rect(x1, starts, x2, ends, col = di$BandColor, border = NA)
	mtext(side=1, line=0, at=xpos, sub('chr','',chr), cex=0.9)	

	# outer
        rect(xpos-0.2, 0, xpos+0.2, ymax, col = NA, border = 1)

	cent = di[di$BandColor=='red',]
	cent
        rect(xpos-0.2, ymax - cent$ChromStart, xpos+0.2, ymax - cent$ChromEnd, col = 'white', border = 'white')
        lines(c(xpos-0.2, xpos+0.2), c(ymax - cent$ChromStart[1], ymax - cent$ChromEnd[2]), col =1)
        lines(c(xpos+0.2, xpos-0.2), c(ymax - cent$ChromStart[1], ymax - cent$ChromEnd[2]), col =1)

	## off-target
	siteichr = subset(sitei, Chromosome==chr)
	siteichr$off = ymax - siteichr$Start
	(nn = nrow(siteichr))
	
        points(rep(xpos, nn), siteichr$off, pch='--', col=ifelse(siteichr$Mismatches==0, 3, 2), cex=2)
	#xposext = xpos + 0.3
        #points(rep(xposext, nn), siteichr$off, pch='--', col=ifelse(siteichr$Mismatches==0, 3, 2), cex=2)

	xpos = xpos+1
    }

dev.off()
}
