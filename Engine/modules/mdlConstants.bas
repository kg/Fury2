Attribute VB_Name = "mdlConstants"
'
'    Engine (Fury² Game Creation System Runtime Engine)
'    Copyright (C) 2003 Kevin Gadd
'
'    This library is free software; you can redistribute it and/or
'    modify it under the terms of the GNU Lesser General Public
'    License as published by the Free Software Foundation; either
'    version 2.1 of the License, or (at your option) any later version.
'
'    This library is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
'    Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public
'    License along with this library; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'

Option Explicit

Global Const c_lngKeyCount As Long = 255

Global Const c_dblPi As Double = 3.14159265358979
Global Const c_dblRadian As Double = 1.74532925199433E-02

Global Const c_lngMaxNameLength As Long = 31

Global Const c_lngMaxViewportX As Long = 20000000
Global Const c_lngMaxViewportY As Long = 20000000
Global Const c_lngNullValue As Long = -32767&
Global Const c_lngNullColor As Long = -32767&
Global Const c_lngMinFramerate As Long = 5&
Global Const c_lngMaxFramerate As Long = 200&
Global Const c_lngMaxFrameskip As Long = 10&
Global Const c_lngDefaultMaxPathWait As Long = 60&
Global Const c_lngDefaultActivationDistance As Long = 12&
Global Const c_sngDefaultVelocityDecay As Single = 0.33!
Global Const c_sngDefaultMaxVelocity As Single = 2!
Global Const c_sngDefaultRepeatDelay As Single = 0.166

Global Const c_lngMinimumMapWidth As Long = 1
Global Const c_lngMinimumMapHeight As Long = 1
Global Const c_lngMaximumMapWidth As Long = 4096
Global Const c_lngMaximumMapHeight As Long = 4096
Global Const c_lngMinimumMapLayers As Long = 1
Global Const c_lngMaximumMapLayers As Long = 64
Global Const c_lngWindowSkinSegmentSize As Long = 16

Global Const c_lngSFXChannelCount As Long = 64&
Global Const c_lngMinScreenWidth As Long = 64&
Global Const c_lngMinScreenHeight As Long = 64&
Global Const c_lngMaxScreenWidth As Long = 2048&
Global Const c_lngMaxScreenHeight As Long = 2048&
Global Const c_lngMaxHistoryLength As Long = 31&
Global Const c_lngMaxPictureSlot As Long = 255&
Global Const c_lngMaxHotspot As Long = 63&
Global Const c_lngMaxSprites As Long = 2048&
Global Const c_lngMaxAnimations As Long = 256&
Global Const c_lngMaxPoseFrames As Long = 256&
Global Const c_lngMaxPoses As Long = 256&
Global Const c_lngMaxAreas As Long = 1024&
Global Const c_lngMaxStates As Long = 128&
Global Const c_lngMaxCameras As Long = 32&
Global Const c_lngMaxMaps As Long = 8&
Global Const c_lngMaxWaypoints As Long = 1024&
Global Const c_lngMaxTimers As Long = 64&
Global Const c_lngMaxCPUUsage As Long = 85&
Global Const c_lngMaxEventQueue As Long = 127&
Global Const c_lngMaxTimerUpdates As Long = 10&
Global Const c_lngMaxCharMappings As Long = 1024&
Global Const c_lngMaxGlyphs As Long = 10240&
