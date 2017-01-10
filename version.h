#ifndef VERSION_H
#define VERSION_H

	//Date Version Types
	static const char V_DATE[] = "10";
	static const char V_MONTH[] = "01";
	static const char V_YEAR[] = "2017";
	static const char V_UBUNTU_VERSION_STYLE[] =  "17.01";
	
	//Software Status
	static const char V_STATUS[] =  "Beta";
	static const char V_STATUS_SHORT[] =  "b";
	
	//Standard Version Type
	static const long V_MAJOR  = 4;
	static const long V_MINOR  = 0;
	static const long V_BUILD  = 70;
	static const long V_REVISION  = 391;
	
	//Miscellaneous Version Types
	static const long V_BUILDS_COUNT  = 116;
	#define V_RC_FILEVERSION 4,0,70,391
	#define V_RC_FILEVERSION_STRING "4, 0, 70, 391\0"
	static const char V_FULLVERSION_STRING [] = "4.0.70.391";
	
	//These values are to keep track of your versioning state, don't modify them.
	static const long V_BUILD_HISTORY  = 70;
	

#endif //VERSION_H
