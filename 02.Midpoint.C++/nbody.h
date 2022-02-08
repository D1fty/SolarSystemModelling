#ifndef NBODY_H
#define NBODY_H

// Standard Headers
#include <iostream>
#include <iomanip>
#include <fstream>

// SFML
#include <SFML/Window.hpp>
#include <SFML/Graphics.hpp>

// STB Image Headers
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"

// Uniform
float uniform() {
	return rand()/(float)RAND_MAX;
}

// ----------------------------------------
// Vector 2D Struct
// ----------------------------------------
struct vec2 {
	// Data Members
	double x, y;

	// Constructor
	vec2(double _x=0, double _y=0) {
		x = _x;
		y = _y;
	}
};

// Operators +
vec2 operator+(const vec2 &a, const vec2 &b) {
	return vec2(a.x + b.x, a.y + b.y);
}
vec2& operator+=(vec2 &a, const vec2 &b) {
	a.x += b.x;
	a.y += b.y;
	return a;
}

// Operators -
vec2 operator-(const vec2 &a, const vec2 &b) {
	return vec2(a.x - b.x, a.y - b.y);
}
vec2& operator-=(vec2 &a, const vec2 &b) {
	a.x -= b.x;
	a.y -= b.y;
	return a;
}

// Operators *
vec2 operator*(const vec2 &a, const double &b) {
	return vec2(a.x * b, a.y * b);
}
vec2 operator*(const double &a, const vec2 &b) {
	return vec2(a * b.x, a * b.y);
}
vec2& operator*=(vec2 &a, const double &b) {
	a.x *= b;
	a.y *= b;
	return a;
}

// Operators /
vec2 operator/(const vec2 &a, const double &b) {
	return vec2(a.x / b, a.y / b);
}
vec2& operator/=(vec2 &a, const double &b) {
	a.x /= b;
	a.y /= b;
	return a;
}

// Operators !
bool operator!(const vec2 &a) {
	if (!a.x && !a.y)
	{
		return true;
	}
	return false;
}

// Functions
double length(const vec2 &a) {
	return sqrt(pow(a.x, 2.0) + pow(a.y, 2.0));
}

double length2(const vec2 &a) {
	return pow(a.x, 2.0) + pow(a.y, 2.0);
}

const vec2 normalise(const vec2 &a) {
	return a / length(a);
}

// ----------------------------------------
// Body Struct
// ----------------------------------------
struct body {
	// Data Members
	vec2		renderPos;
	vec2 		vel;
	vec2  		vel2;
	double 		mass;
	double 		radius;
	double 		renderRadius;
	sf::Color 	renderColour;
	std::string	name;

	// Constructor
	body() {
		vel 	= vec2(0, 0);
		mass 	= 0;
	}

	body(double vx, double vy, double m, double r, double rr, sf::Color rc, std::string n) {
		vel 		 = vec2(vx, vy);
		mass 		 = m;
		radius 		 = r;
		renderRadius = rr;
		renderColour = rc;
		name		 = n;
	}

	void calculateRenderPos(int offsetx, int offsety, vec2 pos)
	{
		renderPos.x = pos.x / 8000000  + offsetx - renderRadius; // Ideal is 12000000000 but inner planets are too close to sun
		renderPos.y = pos.y / 8000000  + offsety - renderRadius; // As above
	}
};

// ----------------------------------------
// Output Functions
// ----------------------------------------
void write_data(const char *filename, int N, vec2 *pos, double *tIL, double *cIL, body *bodies) {
	// Open file
	std::ofstream output(filename);

	output << std::setprecision(10);

	output << "Total Initial Energy: " << *tIL << std::endl;
	output << "Total Current Energy: " << *cIL << std::endl;
	output << "Energy Change: "        << ((*tIL - *cIL) / *tIL) << "%" << std::endl;
	output << std::endl << "Planetary positions:" << std::endl;

	// For each body
	for(int i = 0; i < N; ++i) {
		// Write position
		output << bodies[i].name << ": "; 
		for (int j = 0; j < (9 - bodies[i].name.length()); j++)
		{
			output << " ";
		}
		output << std::setw(16) << pos[i].x << " " << std::setw(16) << pos[i].y << std::endl;
	}

	// Close file
	output.close();
}

void write_image(const char *filename, int N, int width, int height, vec2 *pos) {
	// Generate output image
	unsigned char *image = new unsigned char[width * height * 3];
	memset(image, 0, width * height * 3);

	// For each body
	for(int i = 0; i < N; ++i) {
		// Get Position
		vec2 p = pos[i];

		if(p.x >= 0 && p.x < width && p.y >= 0 && p.y < height) {
			image[((((int)p.y * width) + (int)p.x) * 3)] = 255;
		}
	}

	// Write PNG output
	stbi_write_png(filename, width, height, 3, image, width * 3);

	// Delete image
	delete[] image;
}

#endif