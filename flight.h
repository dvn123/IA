#pragma once

#include "Source.h"

class Flight {
	int plane_id;
	float lower_limit, upper_limit, prefered_time, early_cost, late_cost, time_until_track_can_be_reused;
public:
	Flight(int plane_id, float lower_limit, float upper_limit, float prefered_time, float early_cost, float time_until_track_can_be_reused);

	void set_plane_id(int plane_id);
	int get_plane_id();

	void set_lower_limit(float lower_limit);
	float get_lower_limit();
	void set_upper_limit(float upper_limit);
	float get_upper_limit();
	void set_prefered_time(float prefered_time);
	float get_prefered_time();
	void set_early_cost(float early_cost);
	float get_early_cost();
	void set_late_cost(float late_cost);
	float get_late_cost();
	void set_time_until_track_can_be_reused(float time_until_track_can_be_reused);
	float get_time_until_track_can_be_reused();

	float get_cost(float time);
};