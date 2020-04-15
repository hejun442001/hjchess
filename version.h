#ifndef VERSION_H
#define VERSION_H

	//Date Version Types
	static const char V_DATE[] = "15";
	static const char V_MONTH[] = "04";
	static const char V_YEAR[] = "2020";
	static const char V_UBUNTU_VERSION_STYLE[] =  "20.04";
	
	//Software Status
	static const char V_STATUS[] =  "Beta";
	static const char V_STATUS_SHORT[] =  "b";
	
	//Standard Version Type
	static const long V_MAJOR  = 4;
	static const long V_MINOR  = 0;
	static const long V_BUILD  = 79;
	static const long V_REVISION  = 438;
	
	//Miscellaneous Version Types
	static const long V_BUILDS_COUNT  = 127;
	#define V_RC_FILEVERSION 4,0,79,438
	#define V_RC_FILEVERSION_STRING "4, 0, 79, 438\0"
	static const char V_FULLVERSION_STRING [] = "4.0.79.438";
	
	//These values are to keep track of your versioning state, don't modify them.
	static const long V_BUILD_HISTORY  = 79;
	

#endif //VERSION_H
