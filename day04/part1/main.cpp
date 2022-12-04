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
    int fullyContainedRanges = 0;
    while (inputFile >> firstElveStart >> delimiter >> firstElveEnd >> delimiter >> secondElveStart >> delimiter >> secondElveEnd) {
        if (firstElveStart <= secondElveStart && firstElveEnd >= secondElveEnd)
            fullyContainedRanges++;
        else if (firstElveStart >= secondElveStart && firstElveEnd <= secondElveEnd)
            fullyContainedRanges++;
    }

    std::cout << fullyContainedRanges << std::endl;
    return 0;
}
