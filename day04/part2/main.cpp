#include <iostream>
#include <fstream>

int main() {
    std::ifstream inputFile("./input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Could not open input file!" << std::endl;
        exit(1);
    }

    int firstElveStart, firstElveEnd, secondElveStart, secondElveEnd;
    char delimiter;
    int overlappingRanges = 0;
    while (inputFile >> firstElveStart >> delimiter >> firstElveEnd >> delimiter >> secondElveStart >> delimiter >> secondElveEnd) {
        int firstRangeLength = firstElveEnd - firstElveStart;
        int secondRangeLength = secondElveEnd - secondElveStart;
        int overallRangeStart = std::min(firstElveStart, secondElveStart);
        int overallRangeEnd = std::max(firstElveEnd, secondElveEnd);
        int overallRangeLength = overallRangeEnd - overallRangeStart;
        if ((firstRangeLength + secondRangeLength) >= overallRangeLength)
            overlappingRanges++;
    }

    std::cout << overlappingRanges << std::endl;
    return 0;
}
