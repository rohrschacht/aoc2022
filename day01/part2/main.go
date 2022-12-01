package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
)

func main() {
	currentCalorieSum := 0
	calorieList := make([]int, 0)
	inputFile, err := os.Open("./input.txt")
	if err != nil {
		log.Fatalf("Could not open file! %v", err)
	}

	scanner := bufio.NewScanner(inputFile)
	for scanner.Scan() {
		input := scanner.Text()

		if input == "" {
			calorieList = append(calorieList, currentCalorieSum)
			currentCalorieSum = 0
		} else {
			calories, err := strconv.Atoi(input)
			if err != nil {
				log.Fatalf("Could not convert input to integer! %v %v", input, err)
			}

			currentCalorieSum += calories
		}
	}

	sort.Ints(calorieList)
	lastIndex := len(calorieList) - 1
	totalCaloriesTop3 := calorieList[lastIndex] + calorieList[lastIndex - 1] + calorieList[lastIndex - 2]
	fmt.Println(totalCaloriesTop3)
}
