/*
 * Copyright 2013-2014 Tobii Technology AB. All rights reserved.
 * Modified by MaxiDonkey 06/2021
 */

#include <Windows.h>
#include <stdio.h>
#include <conio.h>
#include <assert.h>
#include <winuser.h>
#include "eyex\EyeX.h"  
#include "windows.h" 
#include <winreg.h>
#include <malloc.h>
#include <stdbool.h>

#pragma comment (lib, "Tobii.EyeX.Client.lib")

void HandleNewCoords(float x, float y);

HWND EliteHnd;
int ScreenMaxWidth = 0;
int ScreenMaxHeight = 0;
int Noise = 250;

int Running = -99;
int WalkDeadZone = 1;
int Sensibility = 20;

float lastRawX = 0;
float lastRawY = 0;

float filteredX = 0;
float filteredY = 0;
float integratedErrorX = 0;
float integratedErrorY = 0;
float integratingSpeed = 1.0f;
const float FIntegrationDeadZone = 16.0; // pixels
const float FDeadZoneX = 8.0;			 // pixels
const float FDeadZoneY = 8.0;			 // pixels
const float g_SlowZone = 80;			 // pixels
float FSpeed = 0.18f;					 // 0.24f; how fast to move when far away
float FSlowSpeed = 0.06f;				 // 0.12f; how fast to move when close
float filteredErrorX = 0;				 // the distance between the gaze data and the current mouse position
float filteredErrorY = 0;				 // the distance between the gaze data and the current mouse position

static const TX_STRING InteractorId = "EliteOdysseyGaze";  

// global variables
static TX_HANDLE g_hGlobalInteractorSnapshot = TX_EMPTY_HANDLE;

void GetScreenDim() {
	ScreenMaxWidth  = GetSystemMetrics(SM_CXSCREEN);
	ScreenMaxHeight = GetSystemMetrics(SM_CYSCREEN);
};
  

/*
* Test registry access
*/

int GetRegistryValue(TX_STRING SubKey, int Default)
{
	DWORD dwData;
	HKEY hKey;
	LONG returnStatus;
	DWORD dwType = REG_DWORD;
	DWORD dwSize = sizeof(DWORD) * 2;

	returnStatus = RegOpenKeyEx(HKEY_CURRENT_USER, L"Software\\RaoulFumier", 0L, KEY_ALL_ACCESS, &hKey);  //L"Software\\GazeMode"
	if (returnStatus == ERROR_SUCCESS)
	{
		returnStatus = RegQueryValueEx(hKey, SubKey, NULL, &dwType, (LPBYTE)&dwData, &dwSize);
		if (returnStatus == ERROR_SUCCESS)
		{
			RegCloseKey(hKey);
			return (int)dwData;
		}
		else {
			RegCloseKey(hKey);
			return Default;
		}
	}
}


SetRegistryValue(TX_STRING SubKey, DWORD Value)
{
	HKEY hKey;
	LONG returnStatus;

	returnStatus = RegOpenKeyEx(HKEY_CURRENT_USER, L"Software\\RaoulFumier", NULL, KEY_ALL_ACCESS, &hKey); //L"Software\\GazeMode"
	if (returnStatus == ERROR_SUCCESS)
	{
		returnStatus = RegSetValueEx(hKey, SubKey, NULL, REG_DWORD, (LPBYTE)&Value, sizeof(Value));
		RegCloseKey(hKey);
	}
}

/*
 * Initializes g_hGlobalInteractorSnapshot with an interactor that has the Gaze Point behavior.
 */
BOOL InitializeGlobalInteractorSnapshot(TX_CONTEXTHANDLE hContext)
{
	TX_HANDLE hInteractor = TX_EMPTY_HANDLE;
	TX_GAZEPOINTDATAPARAMS params = { TX_GAZEPOINTDATAMODE_LIGHTLYFILTERED };
	BOOL success;
	 
	success = txCreateGlobalInteractorSnapshot(
		hContext,
		InteractorId,
		&g_hGlobalInteractorSnapshot,
		&hInteractor) == TX_RESULT_OK;
	success &= txCreateGazePointDataBehavior(hInteractor, &params) == TX_RESULT_OK;
	txReleaseObject(&hInteractor);
	return success;
}

HWND GetEliteHandle() {
	return FindWindowA("FrontierDevelopmentsAppWinClass", NULL);
}

EliteForeGround() {
  if (EliteHnd == 0) { 
	  EliteHnd = GetEliteHandle();
  }
  SetForegroundWindow(EliteHnd);
}

BOOL IsEliteRunnng() {
  EliteHnd = GetEliteHandle();
  if (EliteHnd == 0) { return false; }
  else return true;
}

/*
 * Callback function invoked when a snapshot has been committed.
 */
void TX_CALLCONVENTION OnSnapshotCommitted(TX_CONSTHANDLE hAsyncData, TX_USERPARAM param)
{

	TX_RESULT result = TX_RESULT_UNKNOWN;
	txGetAsyncDataResultCode(hAsyncData, &result);
	assert(result == TX_RESULT_OK || result == TX_RESULT_CANCELLED);
}

/*
 * Callback function invoked when the status of the connection to the EyeX Engine has changed.
 */
void TX_CALLCONVENTION OnEngineConnectionStateChanged(TX_CONNECTIONSTATE connectionState, TX_USERPARAM userParam)
{
	switch (connectionState) {
	case TX_CONNECTIONSTATE_CONNECTED: {
			BOOL success;
			printf("The connection state is now CONNECTED (We are connected to the EyeX Engine)\n");
			success = txCommitSnapshotAsync(g_hGlobalInteractorSnapshot, OnSnapshotCommitted, NULL) == TX_RESULT_OK;
			if (!success) {
				printf("Failed to initialize the data stream.\n");
			}
			else
			{
				printf("Waiting for gaze data to start streaming...\n");
			}
		}
		break;

	case TX_CONNECTIONSTATE_DISCONNECTED:
		printf("The connection state is now DISCONNECTED (We are disconnected from the EyeX Engine)\n");
		break;

	case TX_CONNECTIONSTATE_TRYINGTOCONNECT:
		printf("The connection state is now TRYINGTOCONNECT (We are trying to connect to the EyeX Engine)\n");
		break;

	case TX_CONNECTIONSTATE_SERVERVERSIONTOOLOW:
		printf("The connection state is now SERVER_VERSION_TOO_LOW: this application requires a more recent version of the EyeX Engine to run.\n");
		break;

	case TX_CONNECTIONSTATE_SERVERVERSIONTOOHIGH:
		printf("The connection state is now SERVER_VERSION_TOO_HIGH: this application requires an older version of the EyeX Engine to run.\n");
		break;
	}
}

/*
 * Handles an event from the Gaze Point data stream.
 */
void OnGazeDataEvent(TX_HANDLE hGazeDataBehavior)
{
	TX_GAZEPOINTDATAEVENTPARAMS eventParams;
	if (txGetGazePointDataEventParams(hGazeDataBehavior, &eventParams) == TX_RESULT_OK) { 
		HandleNewCoords((float)eventParams.X, (float)eventParams.Y);
	} else {
		printf("Failed to interpret gaze data event packet.\n");
	}
}


/*
*  From central screen point Relative. Calculate the Move from  and send résult to mouse by mouse_event
*/

void MoveInOdyssey(int x, int y) {
	int a = x;
	int b = y;
	EliteForeGround();
	Sleep(10);
	//if ((b < 150) || (b > 930)) { return; } //dead zones for TaupeEyes TODO
	a = (int)( (ScreenMaxWidth / 2 - a)  / Sensibility );  
	b = (int)( (ScreenMaxHeight / 2 - b) / Sensibility );  
	if ((abs(a) <= WalkDeadZone) && (abs(b) <= WalkDeadZone)) {
		return;
	}
	printf("JOY   %d : %d \n", a, b);
	mouse_event(MOUSEEVENTF_MOVE, -a, -b, 0, 0);
}

void MoveInMenu(int x, int y) {
	printf("MENU   %d : %d \n", x, y);
	SetCursorPos(x, y);
}

void HandleNewCoords(float x, float y)
{
	
	if ((Running != 1) && (Running != 2)) {
		return;
	}

	if (abs(lastRawX - x) + abs(lastRawY - y) > Noise) {
		lastRawX = x;
		lastRawY = y;
		return;
	}
	
	float errorX = x - filteredX;
	float errorY = y - filteredY;

	integratedErrorX += errorX;
	integratedErrorY += errorY;

	float speed = FSlowSpeed;
	if (abs(errorX + errorY) > g_SlowZone) speed = FSpeed;

	if (abs(errorX) > FDeadZoneX){ filteredX += speed * errorX; }
	if (abs(errorY) > FDeadZoneY){ filteredY += speed * errorY; }
	
	if (filteredX < 0) { filteredX = 0; }
	if (filteredY < 0) { filteredY = 0; }
	if (filteredX > ScreenMaxWidth) { filteredX = ScreenMaxWidth; }
	if (filteredY > ScreenMaxHeight) { filteredY = ScreenMaxHeight; }

	if (Running == 1) {
		MoveInOdyssey((int)filteredX, (int)filteredY);
	}
	else
		if (Running == 2) {
			MoveInMenu((int)filteredX, (int)filteredY);
		}	
}

/*
 * Callback function invoked when an event has been received from the EyeX Engine.
 */
void TX_CALLCONVENTION HandleEvent(TX_CONSTHANDLE hAsyncData, TX_USERPARAM userParam)
{
	TX_HANDLE hEvent = TX_EMPTY_HANDLE;
	TX_HANDLE hBehavior = TX_EMPTY_HANDLE;

    txGetAsyncDataContent(hAsyncData, &hEvent);
	if (txGetEventBehavior(hEvent, &hBehavior, TX_BEHAVIORTYPE_GAZEPOINTDATA) == TX_RESULT_OK) {
		OnGazeDataEvent(hBehavior);
		txReleaseObject(&hBehavior);
	}
	txReleaseObject(&hEvent);
}

//Global var
TX_CONTEXTHANDLE hContext = TX_EMPTY_HANDLE;
TX_TICKET hConnectionStateChangedTicket = TX_INVALID_TICKET;
TX_TICKET hEventHandlerTicket = TX_INVALID_TICKET;

BOOL Initialize()
{
	BOOL success;

	// initialize and enable the context that is our link to the EyeX Engine.
	success = txInitializeEyeX(TX_EYEXCOMPONENTOVERRIDEFLAG_NONE, NULL, NULL, NULL, NULL) == TX_RESULT_OK;
	success &= txCreateContext(&hContext, TX_FALSE) == TX_RESULT_OK;
	success &= InitializeGlobalInteractorSnapshot(hContext);
	success &= txRegisterConnectionStateChangedHandler(hContext, &hConnectionStateChangedTicket, OnEngineConnectionStateChanged, NULL) == TX_RESULT_OK;
	success &= txRegisterEventHandler(hContext, &hEventHandlerTicket, HandleEvent, NULL) == TX_RESULT_OK;
	success &= txEnableConnection(hContext) == TX_RESULT_OK;
	return success;
}

Finalize()
{
	SetRegistryValue(L"GazeEnabled", 0);
	// disable and delete the context.
	txDisableConnection(hContext);
	txReleaseObject(&g_hGlobalInteractorSnapshot);
	txShutdownContext(hContext, TX_CLEANUPTIMEOUT_DEFAULT, TX_FALSE);
	txReleaseContext(&hContext);
}

SpeedUpdate() {
	int Combat = GetRegistryValue(L"Combat", 0);

	if (Combat == 0) {
		FSpeed = 0.24f;
		FSlowSpeed = 0.12f;
	}
	else {
		FSpeed = 0.18f;
		FSlowSpeed = 0.06f;
	}
}

Update()
{
	WalkDeadZone = GetRegistryValue(L"WalkDeadZone", 0);
	Sensibility = GetRegistryValue(L"Sensibility", 20);
	if (Sensibility == 10) { Sensibility = 13; }
	Noise = GetRegistryValue(L"Noise", 250);
	SpeedUpdate();
	SetRegistryValue(L"Update", 0);
}


/*
 * Application entry point.
 */
int main(int argc, char* argv[])
{
	GetScreenDim();
	if (Initialize()) {
		//HIDE CONSOLE WINDOW
		//ShowWindow(GetConsoleWindow(), SW_SHOW); 
		ShowWindow(GetConsoleWindow(), SW_HIDE);
		SetRegistryValue(L"GazeEnabled", 1);
		while (Running != 0) {
			Running = GetRegistryValue(L"GazeMode", 0);
			if (Running != 0) {
				BOOL RegModified = (GetRegistryValue(L"Update", 0) == 1);
				if (RegModified == true) { Update(); }
				if (IsEliteRunnng() == false) { Running = 0; }
			}
			Sleep(350);
		}
	}
	Finalize();
	return 0;
}
