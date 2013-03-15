getwd()
hsm <- read.table("output/aggregate.txt", sep=",", header=TRUE, strip.white = TRUE)
options = commandArgs(trailingOnly = TRUE)
# produce a line of data for a data frame
if("table" %in% options) {
	# read current table
	out <- read.table("output/table.txt", sep=",", header=TRUE, strip.white = TRUE)
	# compute new variables
	misses = sum(hsm$misses)
	whiffs = sum(hsm$whiffs)
	totalwhiffs = sum(hsm$totalwhiffs)
	avoids = sum(hsm$avoids)
	accuracy = sum(hsm$hits) / (sum(hsm$hits) + sum(hsm$misses))
	# add variables to table
	out[length(out$misses) + 1,] = c(misses, whiffs, totalwhiffs, avoids, accuracy, options[3], options[4])
	# write table back to file
	write.table(out, sep=",", quote = FALSE, row.names = FALSE, file = "output/table.txt")
}
if("stats" %in% options) {
	print("misses:")
	print(table(hsm$misses))
	print("whiffs:")
	print(table(hsm$whiffs))
	print(sum(hsm$whiffs))
	print("total whiffs:")
	print(table(hsm$totalwhiffs))
	print(sum(hsm$totalwhiffs))
	print(sprintf("accuracy: %f", sum(hsm$hits) / (sum(hsm$hits) +  sum(hsm$misses))))
	str(subset(hsm, complete > 6))
}
if("plots" %in% options) {
	library(ggplot2)

	dir.create("output", showWarnings = FALSE)

	ggplot(hsm, aes(complete, fill=as.factor(hovers))) + geom_histogram(pos="dodge")
	ggsave(file="output/byhover.pdf")

	ggplot(hsm, aes(complete, fill=as.factor(whiffs))) + geom_histogram(pos="dodge")
	ggsave(file="output/bywhiff.pdf")

	ggplot(hsm, aes(complete, fill=as.factor(misses))) + geom_histogram(pos="dodge")
	ggsave(file="output/bymiss.pdf")

	ggplot(hsm, aes(complete, fill=as.factor(avoids))) + geom_histogram(pos="dodge")
	ggsave(file="output/byavoid.pdf")

	ggplot(hsm, aes(complete, fill=as.factor(order))) + geom_histogram(pos="dodge")
	ggsave(file="output/byorder.pdf")

	ggplot(hsm, aes(complete, fill=as.factor(totalwhiffs))) + geom_histogram(pos="dodge")
	ggsave(file="output/bytotalwhiff.pdf")
}