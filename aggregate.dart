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
//	print("hits:");
//	[0, 1, 2, 3].forEach((count) => print("$count: ${hit_counts.where((cur_count) => cur_count == count).length}"));
	// sum hits
	int hits = hit_counts.reduce(0, (cum, next) => cum + next);
//	print("total hits: $hits");

	// get all miss counts
	var miss_counts = new RegExp(r"misses: (\d+)").allMatches(logContents).map((match) => int.parse(match.group(1))).toList();
	int max = miss_counts.max();
//	print("misses");
//	for(int i = 0; i <= max; i++) {
//		print("$i: ${miss_counts.where((cur_count) => cur_count == i).length}");
//	}
	// sum misses
	int misses = miss_counts.reduce(0, (cum,next) => cum + next);
//	print("total misses: $misses");

//	print("total shots: ${misses + hits}");

	// get all friend hover counts
	var friend_hover_counts = new RegExp(r"friend hovers: (\d*)").allMatches(logContents).map((match) => int.parse(match.group(1))).toList();
//	print("$friend_hover_counts");

	// read completion time from log instead of calculating
	var targetingTimes = new RegExp(r"completion time: (\d*)").allMatches(logContents).map((match) => double.parse(match.group(1)) / 1000).toList();

	/*var min = targetingTimes.min();
	print("min: $min");
	max = targetingTimes.max();
	print("max: $max");
	print("mean: ${targetingTimes.reduce(0, (prev, el) => prev + el) / targetingTimes.length}");
	var bucketSize = 0.1;
	for(num i = min; i <= max; i += bucketSize) {
		int count = targetingTimes.where((time) => i < time && time < i + bucketSize).length;
		print("${i.toStringAsPrecision(2)}: ${new String.fromCharCodes([]..insertRange(0, count, "x".codeUnitAt(0)))}");
	}
	print("${targetingTimes}");*/

	print("complete, hits, misses, hovers");
	for(int i = 0; i < hit_counts.length; i++) {
		print("${targetingTimes[i]}, ${hit_counts[i]}, ${miss_counts[i]}, ${friend_hover_counts[i]}");
	}
}