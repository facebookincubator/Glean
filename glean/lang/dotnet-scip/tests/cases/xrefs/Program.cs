/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

using System;

public class Mammal
{
	private int numberOfLegs = 0;
	private string specie = string.Empty;

    public Mammal(){
		specie = "Unknown";
	}
    
	public Mammal(string specie, int numberOfLegs)
	{
		this.numberOfLegs = numberOfLegs;
		this.specie = specie;
	}

	public int NumberOfLegs
	{
		get
		{
			return numberOfLegs;
		}
	}

	public string Specie
	{
		get
		{
			return specie;
		}
	}
}

public class Dog : Mammal
{
	public Dog() : base("Dogs", 4)
	{
	}
}

public class Human : Mammal
{
	public Human() : base("Humans", 2)
	{
	}
}

class ExecuteInheritance {
	static void Main(string[] args) {
        var m = new Mammal();
		Console.WriteLine($"{m.Specie} has {m.NumberOfLegs} legs");
        
        var d = new Dog();
        Console.WriteLine($"{d.Specie} has {d.NumberOfLegs} legs");

        var h = new Human();
        Console.WriteLine($"{h.Specie} has {h.NumberOfLegs} legs");
   }
}
