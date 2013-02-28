#!/usr/bin/env dart
import "dart:io";

main() {
	// get args
	var args = new Options().arguments;
	// get file name
	var fileString = args.length > 0 ? args[0] : "log.txt";
	// read contents of file
	var logContents = new File(fileString).readAsStringSync();

	// get all hit counts
	var hit_counts = new RegExp(r"hits: (\d*)").allMatches(logContents).map((match) => int.parse(match.group(1))).toList();
	// count each number of hits
	print("hits:");
	[0, 1, 2, 3].forEach((count) => print("$count: ${hit_counts.where((cur_count) => cur_count == count).length}"));
	// sum hits
	int hits = hit_counts.reduce(0, (cum, next) => cum + next);
	print("total hits: $hits");

	// get all miss counts
	var miss_counts = new RegExp(r"misses: (\d+)").allMatches(logContents).map((match) => int.parse(match.group(1)));
	int max = miss_counts.max();
	print("misses");
	for(int i = 0; i <= max; i++) {
		print("$i: ${miss_counts.where((cur_count) => cur_count == i).length}");
	}
	// sum misses
	int misses = miss_counts.reduce(0, (cum,next) => cum + next);
	print("total misses: $misses");

	print("total shots: ${misses + hits}");

	// get all friend hover counts
	var friend_hover_counts = new RegExp(r"friend hovers: (\d*)").allMatches(logContents).map((match) => int.parse(match.group(1))).toList();
	// sum frind hovers
	//int hovers = friend_hover_counts.reduce(0, (cum, next) => cum + next);
	// TODO NO! we need to pair with times.
	print("$friend_hover_counts");


	// get times to hit target
	var lines = logContents.split("\n");
	var state = "hit";
	var match;
	RegExp hitRE = new RegExp(r"hit a target at ([\d\.]*)");
	List<num> hitTimes = [];
	List<num> targetingTimes = [];
	num lastHitTime;
	// iterate over lines
	lines.forEach((line) {
//		print(line);
		if(state == "firsthit") {
			match = hitRE.firstMatch(line);
			if(match != null) {
				hitTimes.add(double.parse(match.group(1)));
				lastHitTime = hitTimes.last;
				state = "hit";
			}
		} else if(state == "hit") {
			match = hitRE.firstMatch(line);
			if(match != null) {
				num time = double.parse(match.group(1));
				//print(lastHitTime);
				hitTimes.add(time - lastHitTime);
				targetingTimes.add(time);
				lastHitTime = time;
			} else if(line.startsWith("time:")) {
				state = "firsthit";
			}
		}
	});

	// read completion time from log instead of calculating
	targetingTimes = new RegExp(r"completion time: (\d*)").allMatches(logContents).map((match) => double.parse(match.group(1)) / 1000).toList();

	var min = targetingTimes.min();
	print("min: $min");
	max = targetingTimes.max();
	print("max: $max");
	print("mean: ${targetingTimes.reduce(0, (prev, el) => prev + el) / targetingTimes.length}");
	var bucketSize = 0.1;
	for(num i = min; i <= max; i += bucketSize) {
		int count = targetingTimes.where((time) => i < time && time < i + bucketSize).length;
		print("${i.toStringAsPrecision(2)}: ${new String.fromCharCodes([]..insertRange(0, count, "x".codeUnitAt(0)))}");
	}
	print("${targetingTimes}");
}