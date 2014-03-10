#include "flight.h"

Flight::Flight(int plane_id, float lower_limit, float upper_limit, float prefered_time, float early_cost, float time_until_track_can_be_reused) {
	this->plane_id = plane_id;
	this->lower_limit = lower_limit;
	this->upper_limit = upper_limit;
	this->prefered_time = prefered_time;
	this->early_cost = early_cost;
	this->time_until_track_can_be_reused = time_until_track_can_be_reused;
}

void Flight::set_plane_id(int plane_id) {
	this->plane_id = plane_id;
}
int Flight::get_plane_id() {
	return plane_id;
}
void Flight::set_lower_limit(float lower_limit) {
	this->lower_limit = lower_limit;
}
float Flight::get_lower_limit() {
	return lower_limit;
}
void Flight::set_upper_limit(float upper_limit) {
	this->upper_limit = upper_limit;
}
float Flight::get_upper_limit() {
	return upper_limit;
}
void Flight::set_prefered_time(float prefered_time) {
	this->prefered_time = prefered_time;
}
float Flight::get_prefered_time() {
	return prefered_time;
}
void Flight::set_early_cost(float early_cost) {
	this->early_cost = early_cost;
}
float Flight::get_early_cost() {
	return early_cost;
}
void Flight::set_late_cost(float late_cost) {
	this->late_cost = late_cost;
}
float Flight::get_late_cost() {
	return late_cost;
}
void Flight::set_time_until_track_can_be_reused(float time_until_track_can_be_reused) {
	this->time_until_track_can_be_reused = time_until_track_can_be_reused;
}
float Flight::get_time_until_track_can_be_reused() {
	return time_until_track_can_be_reused;
}

float Flight::get_cost(float time) {
	if (time > upper_limit || time < lower_limit)
		return -1;
	else if (time > prefered_time)
		return early_cost*(prefered_time - time);
	else 
		return late_cost*(time - prefered_time);
}