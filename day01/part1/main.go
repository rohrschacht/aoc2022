package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	currentCalorieSum := 0
	maxCalorieSum := 0
	inputFile, err := os.Open("./input.txt")
	if err != nil {
		log.Fatalf("Could not open file! %v", err)
	}

	scanner := bufio.NewScanner(inputFile)
	for scanner.Scan() {
		input := scanner.Text()

		if input == "" {
			if currentCalorieSum > maxCalorieSum {
				maxCalorieSum = currentCalorieSum
			}
			currentCalorieSum = 0
		} else {
			calories, err := strconv.Atoi(input)
			if err != nil {
				log.Fatalf("Could not convert input to integer! %v %v", input, err)
			}

			currentCalorieSum += calories
		}
	}

	fmt.Println(maxCalorieSum)
}
