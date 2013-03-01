package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strconv"
)

func main() {
	var filename string

	// get file name from command line
	flag.StringVar(&filename, "file", "log.txt", "Name of the log file")

	file, _ := os.Open(filename)

	buffer, _ := ioutil.ReadAll(bufio.NewReader(file))
	var logContents = string(buffer)

	file.Close()

	hit_re, _ := regexp.Compile(`hits: (\d*)`)
	hits := hit_re.FindAllStringSubmatch(logContents, -1)

	miss_re, _ := regexp.Compile(`misses: (\d*)`)
	misses := miss_re.FindAllStringSubmatch(logContents, -1)

	friend_hover_re, _ := regexp.Compile(`friend hovers: (\d*)`)
	friend_hovers := friend_hover_re.FindAllStringSubmatch(logContents, -1)

	complete_re, _ := regexp.Compile(`completion time: (\d*)`)
	complete := complete_re.FindAllStringSubmatch(logContents, -1)

	whiff_re, _ := regexp.Compile(`whiffs: (\d*)`)
	whiffs := whiff_re.FindAllStringSubmatch(logContents, -1)

	vis_fail_re, _ := regexp.Compile(`vis fails: (\d*)`)
	vis_fails := vis_fail_re.FindAllStringSubmatch(logContents, -1)

	fmt.Println("complete, hits, misses, hovers, whiffs, fails")
	for i := 0; i < len(hits); i++ {
		c, _ := strconv.ParseFloat(complete[i][1], 10)
		fmt.Printf("%f, %v, %v, %v, %v, %v\n", c/1000, hits[i][1], misses[i][1],
			friend_hovers[i][1], whiffs[i][1], vis_fails[i][1])
	}
}
