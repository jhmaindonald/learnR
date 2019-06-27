gcol1 <-
  function(device=""){
    if(device!="")hardcopy(device=device, pointsize=10,
                           width=6, height=6.25, family="Times",
                           fonts=c("Courier","Times"))
    oldpar <- par(mar = c(0,0,2.75,0), oma=c(0.2,0.5,0.2,0.5), xaxs="i",
                  xaxt="n", yaxt="n", ann=FALSE, cex.main=1.2)
    on.exit(par(oldpar))

headA <- "A: Plot symbols and text; specify colors and/or character expansion"
code10 <- "par(fig=c(0, 1, 0.45, 1))"
a1 <- c('plot(1, 1, xlim=c(0, 13.25), ylim=c(0.5, 18.5), type="n")',
           "xpos <- rep((0:12)+0.5, 2);  ypos <- rep(c(14,12), c(13,13))",
           "points(xpos, ypos, cex=3, col=1:26, pch=0:25)",
           "text(xpos, ypos, labels=0:25, cex=0.8)")
a2 <- c("## Plot characters, vary cex (expansion)",
        'text((0:5)+0.5, rep(8, 6), letters[1:6], cex=c(3, 2, 1.5, 1.5, 2, 3))')
a3 <- c("## Position label with respect to point",
        'xoff <- c(0, -0.5, 0, 0.5); yoff <- c(-1,0,1,0)',
        'col4 <- colors()[c(52, 116, 547, 610)]',
        'points(11.1+xoff, 5.7+yoff, pch=16, cex=1.5, col=col4)',
        'posText <- c("below: pos=1", "left: 2", "above: 3", "right: 4")',
        'text(11.1+xoff, 5.7+yoff, posText, pos=1:4)',
        'rect(9.1, 3.3, 13.1, 8.1, border="red")')
eval(parse(text=code10))
for(i in 1:length(a1))eval(parse(text=a1[i]))
eval(parse(text=a2[2]))
for(i in 2:length(a3))eval(parse(text=a3[i]))
par(family="Times")
title(main=headA, adj=0, line=1.75, cex=1.4)
par(family="Courier")
mtext(side=3, line=0.3, code10, adj=0.015)
for(i in 1:13)text(0.15, 19.5-c(1:4,9:10,13:19)[i], c(a1,a2,a3)[i], adj=0)

headB <- 'B: Polygon (triangle), circle, and mathematical text'
code10 <- "par(fig=c(0, 1, 0, 0.42), new=TRUE)"
b1 <- 'plot(1, 1, xlim=c(0, 13.25), ylim=c(0.4, 13.1), type="n")' 
b2 <- c('## Draw a triangle',
        'polygon(x=c(9.3,13,11), y=c(7,8,12), col="gray")')
b3 <- c('## Draw a circle, overlay 2-headed arrow (code=3)',
        'symbols(11.1, 3.7, circles=1.0, bg="gray", add=TRUE, ',
        '        inches=FALSE)',
        'arrows(11.1, 3.7, 12.1, 3.7, length=.05, code=3)')
b4 <- c('## Use expressions to add labeling information ',
        'text(11.6, 3.7-0.75*strheight("R"), expression(italic(r)))',
        'text(11.1, 5.2, expression("Area" == pi*italic(r)^2))',
        'rect(xleft=9.1, ybottom=0.5,',
        '     xright=13.1, ytop=12.4, border="red")')        
eval(parse(text=code10))
eval(parse(text=b1))
par(family="Times")
title(main=headB, adj=0, line=1.75, cex=1.4)
par(family="Courier")
mtext(side=3, line=0.3, code10, adj=0.015)
len <- sum(sapply(list(b1,b2,b3),length))+2
for(i in 1:12)text(0.15, 14-c(1,2:3+0.5, 4:7+1, 8:12+1.5)[i], c(b1,b2,b3,b4)[i], adj=0)
eval(parse(text=b2[2]))
b3 <- c(paste(b3[2:3],collapse=''),b3[4])
eval(parse(text=paste(b3, collapse=';')))
b4 <- c(b4[2:3],paste(b4[4:5],collapse=''))
eval(parse(text=paste(b4, collapse=';')))
if(device!="")dev.off()
  }
