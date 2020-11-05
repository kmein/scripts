#!/usr/bin/env python3
import sys


def insertion_sort(arr, compare=lambda x, y: x < y):
    for i in range(1, len(arr)):
        key = arr[i]
        j = i - 1
        while compare(key, arr[j]):
            arr[j + 1] = arr[j]
            j -= 1
        arr[j + 1] = key
    return arr


def manual_compare(x, y):
    reply = input("{} <= {} (y/n)? ".format(x, y))
    return reply.lower() == "y"


if __name__ == "__main__":
    lines = sys.argv[1:]
    sorted_lines = insertion_sort(lines, manual_compare)
    for i, line in enumerate(lines):
        print(i + 1, line)
