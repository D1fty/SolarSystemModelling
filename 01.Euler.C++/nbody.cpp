// System Headers
#include <iostream>
#include <cmath>
#include <chrono>
#include <omp.h>

// Project Headers
#include "nbody.h"
#include <SFML/Window.hpp>
#include <SFML/Graphics.hpp>

#define MAXSTEP 365

// Number of particles
const int N 		= 10;

// Constants
const double min2 	= 2.0;
const double G 		= 6.67408e-11 * (86400.0*86400.0) / (1000.0*1000.0*1000.0); 	// Einsteins Gravitational Constant per day (m^3 kg^-1 s^-3)
const double dt 	= 1;															// Time step is one day

// Size of Window/Output image
const int width 	= 1920;
const int height 	= 1080;

// Bodies
body 	bodies[N];

// Energies
volatile double  step 		 	= 0;
double  totalInitialEnergy		= 0;
double  potentialEnergies[N];
double	kineticEnergies[N];

// Update Nbody Simulation
double update()
{
	// Acceleration
	vec2 acc[N];
	int i;

	// Clear energies
	for (i = 0; i < N; ++i)
	{
		acc[i] 				 = vec2(0, 0);
		potentialEnergies[i] = 0;
	}

	// For each body
	for (i = 0; i < N; ++i)
	{
		// For each following body
		for (int j = i + 1; j < N; ++j)
		{
			// Difference in position
			vec2 dx = bodies[i].pos - bodies[j].pos;

			// Normalised difference in position
			vec2 u = normalise(dx);

			// Calculate distance squared
			double d2 = length2(dx);

			// If greater than minimum distance
			if (d2 > min2)
			{
				// Force between bodies
				double f = -G * bodies[i].mass * bodies[j].mass / d2;

				// Add to acceleration
				acc[i] += (u * f / bodies[i].mass);
				acc[j] -= (u * f / bodies[j].mass);

				// Add to potential Energies
				double U = (-G * bodies[i].mass * bodies[j].mass / sqrt(d2)) * 0.5;
				potentialEnergies[i] += U;
				potentialEnergies[j] += U;
			}
		}
	}

	// Sum energy of System
	if (step == 0)
	{
		// Initial Energy (starting velocities plus total gravitational potential energy)
		for (i = 0; i < N; i++)
		{
			totalInitialEnergy += potentialEnergies[i];
			totalInitialEnergy += kineticEnergies[i];
		}
	}

	//  Current potential energy based off last instantaneous velocity and new accelleration
	double currentTotalEnergy = 0;
	for (i = 0; i < N; i++)
	{
		currentTotalEnergy += potentialEnergies[i];
		currentTotalEnergy += kineticEnergies[i];
	}
	
	// Print to console energy calculation
	std::cout << "Step: " << step << std::endl;
	std::cout << "Initial total energy: " << totalInitialEnergy << std::endl;
	std::cout << "Current total energy: " << currentTotalEnergy << std::endl; 

	// For each body
	for (i = 0; i < N; ++i)
	{
		// Update Position
		bodies[i].pos += bodies[i].vel * dt;

		// Update Velocity
		bodies[i].vel += acc[i] * dt;

		// Energy Calculations
		double vel 			 = sqrt((bodies[i].vel.x * bodies[i].vel.x) + (bodies[i].vel.y * bodies[i].vel.y)); 	// velocity is a vector constructed from vx and vy
		kineticEnergies[i]   = 0.5 * (bodies[i].mass) * (vel*vel); 													// KE = half * Mass * Vel^2														
	}

	// Incriment the step counter
	step++;

	// Return the energy of the system
	return currentTotalEnergy;
}

// Initialise NBody Simulation with planets 
void initialise()
{
	// Planet bodies and their starting conditions
	// Parameters in order: 
		// Distance from sun x	KM
		// Distance from sun y 	KM
		// Orbital velocity x 	KM/Day
		// Orbital velocity y 	KM/Day
		// Mass 				KG
		// Radius 				KM
		// Render Radius		Pixels
		// Color

	// Sun
	bodies[0] = body(0, 0, 0, 0, 1.989e30, 696340, 4, sf::Color(168, 149, 50), "Sun");

	// Mercury
	bodies[1] = body(5.791e7, 0, 0, (double)-4138560, 3.285e23, 2439.7, 2, sf::Color(115,  21,   7), "Mercury"); 
	
	// Venus
	bodies[2] = body(1.082e8, 0, 0, (double)-3024000, 4.867e24, 6051.8, 2, sf::Color(115,  97,   7), "Venus");
	
	// Earth
	bodies[3] = body(1.496e8, 0, 0, (double)-2574720, 5.972e24, 6371, 2, sf::Color(  7,  25, 115), "Earth");
	
	// Mars
	bodies[4] = body(2.279e8, 0, 0, (double)-2082240, 6.390e23, 3389.5, 2, sf::Color(115,  27,   7), "Mars"); 
	
	// Jupiter
	bodies[5] = body(7.785e8, 0, 0, (double)-1131840, 1.898e27, 69911, 2, sf::Color( 69,  61,  59), "Jupiter");
	
	// Saturn
	bodies[6] = body(1.434e9, 0, 0,  (double)-838080, 5.683e26, 58232, 2, sf::Color(128,  94,   3), "Saturn");
	
	// Uranus
	bodies[7] = body(2.871e9, 0, 0,  (double)-587520, 8.681e25, 25362, 2, sf::Color( 10, 134, 196), "Uranus");
	
	// Neptune
	bodies[8] = body(4.495e9, 0, 0,  (double)-466560, 1.024e26, 24622, 2, sf::Color( 10,  25, 196), "Neptune");
	
	// Pluto
	bodies[9] = body(5.900e9, 0, 0,  (double)-403488, 1.309e22, 1188.3, 2, sf::Color(138, 138, 138), "Pluto");

	// Calculate their initial Kinetic Energies
	for (int i = 0; i < N; i++)
	{
		double vel 			 = sqrt((bodies[i].vel.x * bodies[i].vel.x) + (bodies[i].vel.y * bodies[i].vel.y)); 	
		kineticEnergies[i]   = 0.5 * (bodies[i].mass) * (vel*vel); 
	}
}

// Main Function - Benchmark
int main()
{
	// Initialise NBody Simulation
	initialise();
	double totalCurrentEnergy = 0;

	// Get start time
	printf("Running simulation...\n");
	std::chrono::system_clock::time_point start = std::chrono::system_clock::now();

	// Run Simulation
	while (step < MAXSTEP)
	{
		// Update NBody Simluation
		totalCurrentEnergy = update();
	}

	// Write position data
	printf("Writing output file...\n");
	write_data("output.dat", N, &totalInitialEnergy, &totalCurrentEnergy, bodies);

	// Get end time
	std::chrono::system_clock::time_point end = std::chrono::system_clock::now();
	printf("Finished simulation...\n");

	// Time Taken
	printf("Calculating time taken...\n");
	std::cout << "Time Taken: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() / 1000000.0 << std::endl;
}