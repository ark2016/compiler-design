package main

import "fmt"

func main() {
	fmt.Println("Start tests!")
	{
		var __labCounter int

		for i := 0; i < 3; i++ {
			__labCounter++
			fmt.Println("loop #1, iteration:", i)
		}
		fmt.Println("Iterations:", __labCounter)
	}

	j := 0
	{
		var __labCounter int
		for j < 2 {
			__labCounter++
			fmt.Println("loop #2, iteration:", j)
			j++
		}
		fmt.Println("Iterations:", __labCounter)
	}

	k := 0
	{
		var __labCounter int
		for {
			__labCounter++
			if k >= 2 {
				break
			}
			fmt.Println("loop #3, iteration:", k)
			k++
		}
		fmt.Println("Iterations:", __labCounter)
	}
	{
		var __labCounter int

		for i := 0; i < 3; i++ {
			__labCounter++
			{
				var __labCounter int
				for j := 1; j < 3; j++ {
					__labCounter++
					fmt.Printf("loop #%d, iteration: %d\n", i, j)
				}
				fmt.Println("Iterations:", __labCounter)
			}
		}
		fmt.Println("Iterations:", __labCounter)
	}

	fmt.Println("All test finished successful!")
}
