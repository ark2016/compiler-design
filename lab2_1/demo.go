package main

import "fmt"

func main() {
	fmt.Println("Start tests!")

	for i := 0; i < 3; i++ {
		fmt.Println("loop #1, iteration:", i)
	}

	j := 0
	for j < 2 {
		fmt.Println("loop #2, iteration:", j)
		j++
	}

	k := 0
	for {
		if k >= 2 {
			break
		}
		fmt.Println("loop #3, iteration:", k)
		k++
	}

	for i := 0; i < 3; i++ {
		for j := 1; j < 3; j++ {
			fmt.Printf("loop #%d, iteration: %d\n", i, j)
		}
	}

	fmt.Println("All test finished successful!")
}
