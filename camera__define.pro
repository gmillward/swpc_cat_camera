;+
; NAME:
;       CAMERA__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a camera object
;       class for use in rendering 3d transforms in IDL object graphics.
;
;       The camera object allows intuitive control of object graphics scene
;       composition, something that was impossible for me using IDL's
;       built in model methods.
;
;       There are two basic methods for specifying the camera's view,
;       the axis-angle method and the look-at method.  The axis-angle
;       method defines the orientation as a series of rotations about
;       x, y, and z, axes.  The look-at method centers the view on the
;       defined look at point.
;
;
; AUTHOR:
;       Rick Towler
;       School of Aquatic and Fishery Sciences
;       University of Washington
;       Box 355020
;       Seattle, WA 98195-5020
;       rtowler@u.washington.edu
;       www.acoustics.washington.edu
;
;
; CATEGORY:
;       Object Graphics
;
;
; CALLING SEQUENCE:
;       camera = Obj_New("camera", [, CAMERA_LOCATION{Get, Set}=[x,y,z]]
;               [, COLOR{Get, Set}=index or RGB vector] [, DEPTH_CUE{Get, Set}=[zbright, zdim]]
;               [, DIMENSIONS{Get, Set}=[width, height]] [, /DOUBLE {Get, Set}]
;               [, EYE{Get, Set}=distance] [, LOCATION{Get, Set}=[x, y]] [, /LOCK{Get, Set]
;               [, PITCH{Get, Set}=value{0 to 360}] [, PROJECTION{Get, Set}={1 | 2}]
;               [, ROLL{Get, Set}=value{0 to 360}] [,THIRD_PERSON{Get, Set}=value]
;               [, /TRACK{Get, Set}] [, /TRANSPARENT{Get, Set}] [, UNITS{Get, Set}={0 | 1 | 2 | 3}]
;               [, UVALUE{Get, Set}=value] [, VIEWPLANE_RECT{Get, Set}=[x, y, width, height]]
;               [, YAW{Get, Set}=value{0 to 360}] [, ZCLIP{Get, Set}=[near, far]])
;
;
;
; KEYWORDS:
;
;   lock:           Set this keyword to lock the camera to prevent rolling when
;                   simultaneously changing the pitch and yaw while panning. This
;                   roll is a result of normal quaternion rotation but can be
;                   disorienting and by default is suppressed.  If you desire true
;                   quaternion rotation, set this keyword to 0.
;
;                   Default: 1
;
;   camera_location:Set this keyword to a 3 element vector [x,y,z] that defines
;                   the camera's position in world space.
;
;                   Default: [0.,0.,1.]
;
;   pitch:          Set this keyword to a scalar defining the pitch (rotation about the
;                   X axis) of the camera in degrees. 0 > pitch < 360
;
;                   Default: 0.0
;
;   roll:           Set this keyword to a scalar defining the roll (rotation about the
;                   Z axis) of the camera in degrees. 0 > roll < 360
;
;                   Default: 0.0
;
;   third_person:   Set this keyword to a scalar defining the number of units the
;                   camera will lag behind the defined camera position.  This creates
;                   a simple 3rd person effect. Setting this keyword equal to 0 will
;                   disable it.
;
;                   Default: 0.0
;
;   viewplane_rect: Set this keyword to a four-element vector of the form
;                   [x, y, width, height] to describe the bounds in x and y of the view
;                   volume. Objects within the camera's view volume are projected into
;                   the viewport.  With the camera, the location of the viewplane
;                   rectangle in the X-Y plane is MOSTLY irrelevant.  The location is
;                   only used to calculate the default initial location of the camera.
;                   The width and height are important as they define the field of view
;                   (FOV) of the camera. Smaller values restrict the FOV corresponding
;                   to lenses with longer focal lengths and larger values widen the
;                   FOV like short focal length lenses.
;
;                   Default: [-1.0,-1.0,2.0,2.0]
;
;   yaw:            Set this keyword to a scalar defining the yaw (rotation about the
;                   Y axis) of the camera in degrees. 0 > yaw < 360
;
;                   Default: 0.0
;
;   zoom:           Set this keyword to a scalar defining the zoom factor of the
;                   camera.  Setting zoom < -1.0 will zoom the camera out (decreasing
;                   focal length) while setting zoom > 1.0 will zoom it in (increasing
;                   focal length).  The default focal length of the camera (zoom = 1.0)
;                   depends on the width and height of viewplane_rect.
;
;                   Default: 1.0
;
;
; METHODS:
;
;   This object inherits methods from it's superclass, IDLgrView, and adds the
;   following intrinsic methods:
;
;
;   GetDirectionVector: This function method returnss a unit vector representing
;                       the current camera orientation.
;
;
;   Lookat:         This procedure method changes the camera's orientation by
;                   calculating a direction vector from the camera's position
;                   to a given point in space, a.k.a. the lookat point. The
;                   lookat point is in world units.
;
;                   Set the /TRACK keyword to force the camera to follow your
;                   lookat point thru subsequent transformations.
;
;                   oCamera -> Lookat, [5,0,3]
;
;
;   Pan:            This procedure method changes the camera's orientation by
;                   changing it's pitch and yaw.  The values passed are the
;                   changes in pitch and yaw in degrees.
;
;                   oCamera -> Pan, deltaYaw, deltaPitch
;
;
;   Roll:           This procedure method roll's the camera about the z axis.
;                   The value passed is the change in roll in degrees. Positive
;                   values roll clockwise, negative values roll counterclockwise.
;
;                   oCamera -> Roll, 10.0
;
;
;   Truck:          This procedure method 'moves' the camera along a defined axis
;                   relative to the current orientation.  You must define
;                   the axis to move along [x, y, z] and a distance to move the
;                   camera in world units.
;
;                   To move the camera forward 10 units:
;
;                   oCamera -> Truck, [0, 0, 1], 10.
;
;
;   Zoom:           This procedure method "zooms" the camera by specified zoom
;                   factor. The default view has a zoom factor of 1. The maximum
;                   zoom factor is unlimited but depending on your view coordinates
;                   a practical max is around 50 or 100.
;
;                   To zoom in the camera so that your subject is 5 times it's size:
;
;                   oCamera -> Zoom, 5.
;
;
;
;   A note on the /NO_TRANSFORM keyword: Many of the methods allow you to set the
;   /NO_TRANSFORM keyword which will skip updating the model's transform matrix.
;   Use this when you want to invoke more than one method before updating and drawing
;   a scene.  For example:
;
;       oCamera -> Pan, 10, 0, /NO_TRANSFORM
;       oCamera -> Truck, 0, 0, 1, 10.. /NO_TRANSFORM
;       oCamera -> Roll, 15.
;       oWindow -> draw, oCamera
;
;   The above example would rotate the camera about the y axis 10 degrees to the left,
;   move the camera forward 10 units, roll the camera about the z axis 15 degrees
;   counterclockwise and update the models' transform matricies, then draw the scene.
;
;
; DEPENDENCIES:     quaternion__define.pro
;
;
; EXAMPLE:
;
;                   Please see the HTML documentation for examples.
;
;
; MODIFICATION HISTORY:
;       Written by: Rick Towler, 27 December 2000.
;                   RHT 01/01/01: Added Lookat method.
;                   RHT 01/16/01: Added track and no_transform keywords
;                   RHT 04/30/01: Added view parameter to simplify camera
;                                 setup.  In the process the zclip keyword
;                                 was removed and the camera isn't limited
;                                 to views centered upon the origin.
;                   RHT 06/09/01: Added Zoom method.
;                                 Removed model and view parameters and
;                                 finally made it a child of the IDLgrView
;                                 object.
;                   RHT 08/13/01: Added GetDirectionVector method.
;                   RHT 03/18/02: Fixed small bug in Truck method.
;                   RHT 09/25/02: Fixed small bug in GetProperty where
;                                 the initial vewplane_rect was returned
;                                 instead of the current viewplane_rect.
;
;
;
; LICENSE
;
;   CAMERA__DEFINE.PRO Copyright (C) 2001  Rick Towler
;
;   This program is free software; you can redistribute it and/or
;   modify it under the terms of the GNU General Public License
;   as published by the Free Software Foundation; either version 2
;   of the License, or (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program; if not, write to the Free Software
;   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;   02111-1307, USA.
;
;   A full copy of the GNU General Public License can be found on
;   line at http://www.gnu.org/copyleft/gpl.html#SEC1
;
;-


function Camera::Init,  camera_location=cameraLocation, $
                        lock=lock, $
                        lookat=lookat, $
                        pitch=pitch, $
                        roll=roll, $
                        third_person=thirdPerson, $
                        track=track, $
                        yaw=yaw, $
                        zoom=zoom, $
                        _extra=extra



    if (N_ELEMENTS(lookat) ne 3) then lookat = [0.,0.,0.]
    lock = (N_ELEMENTS(lock) eq 0) ? 1 : KEYWORD_SET(lock)
    pitch = (N_ELEMENTS(pitch) eq 0) ? 0. : 0. > pitch < 360.
    roll = (N_ELEMENTS(roll) eq 0) ? 0. : 0. > roll < 360.
    if (N_ELEMENTS(thirdPerson) eq 0) then thirdPerson = 0.
    track = (N_ELEMENTS(track) eq 0) ? 0 : KEYWORD_SET(track)
    yaw = (N_ELEMENTS(yaw) eq 0) ? 0. : 0. > yaw < 360.
    if (N_ELEMENTS(zoom) eq 0) then zoom = 1.

    self.rotQuat = OBJ_NEW('quaternion')
    self.truckQuat = OBJ_NEW('quaternion')
    self.orientation = OBJ_NEW('quaternion', PITCH=pitch, YAW=yaw, ROLL=roll)

    self.lock = lock
    self.pitch = pitch
    self.roll = roll
    self.thirdPerson = thirdPerson
    self.track = track
    self.yaw = yaw
    self.zoom = zoom

    ok = self -> IDLgrView::Init(_EXTRA=extra)
    if (not ok) then RETURN, 0

    self -> IDLgrView::GetProperty, VIEWPLANE_RECT=viewplaneRect, ZCLIP=zclip
    self.viewRect = viewplaneRect
    self.viewcoord[0] = ((2. * self.viewRect[0]) + self.viewRect[2]) / 2.
    self.viewcoord[1] = ((2. * self.viewRect[1]) + self.viewRect[3]) / 2.
    self.viewcoord[2] = zclip[0] - thirdPerson

    if (N_ELEMENTS(cameraLocation) ne 3) then cameraLocation = $
        [self.viewcoord[0],self.viewcoord[1],1.]
    self.cameraLocation = cameraLocation

    if (self.zoom gt 1.0) or (self.zoom lt -1.0) then self -> Zoom, self.zoom

    RETURN, 1

end


pro Camera::Transform
    ;  Apply the current transform to the models contained in the camera.

    compile_opt idl2

    if (self.track) then begin
        self -> Lookat, self.lookat, /NO_TRANSFORM
        rotation = self.Orientation -> GetCTM()
    endif else begin
        rotation = self.Orientation -> GetCTM()
    endelse

    translation = [[1., 0., 0., -self.cameraLocation[0]], $
                   [0., 1., 0., -self.cameraLocation[1]], $
                   [0., 0., 1., -self.cameraLocation[2]], $
                   [0., 0., 0., 1.]]

    transform = translation # rotation

    translation = [[1., 0., 0., self.viewcoord[0]], $
                   [0., 1., 0., self.viewcoord[1]], $
                   [0., 0., 1., self.viewcoord[2]], $
                   [0., 0., 0., 1.]]

    transform = TEMPORARY(transform) # translation

    models = self -> Get(/all, COUNT=nModels)
    if (nModels lt 0) then RETURN

    for n=0, nModels-1 do models[n] -> SetProperty, TRANSFORM = transform

end


pro Camera::Add, model, _extra=extra
    ;as models are added, we need to update their transform

    self -> IDLgrView::Add, model, _EXTRA=extra

    self -> Transform

end


pro Camera::Lookat, lookat, $
                    no_transform=noTransform, $
                    track=track

    ;  Center the camera view at a specified "lookat" point.

    compile_opt idl2

    if (N_ELEMENTS(lookat) ne 3) then begin
        MESSAGE, 'Lookat must be a 3 element vector in the form [x,y,z]', $
            /CONTINUE
        RETURN
    endif

    if (N_ELEMENTS(track) ne 0) then self.track = KEYWORD_SET(track)
    self.lookat = lookat

    lvector = lookat - self.cameraLocation
    lvn = total(lvector^2)
    if (lvn eq 0.) then RETURN
    lvector = lvector / sqrt(lvn)

    self.yaw = 180. + atan(lvector[0],lvector[2]) * !RADEG
    self.pitch = atan(lvector[1], sqrt(lvector[2]^2 + lvector[0]^2)) * !RADEG

    self.Orientation -> Set, self.pitch, self.yaw, self.roll

    if (not KEYWORD_SET(noTransform)) then self -> Transform

end


pro Camera::Zoom, zoom
    ;  Zoom the camera view by the specified zoom factor.

    compile_opt idl2

    if (N_ELEMENTS(zoom) eq 1) then begin
        case 1 of
            (zoom lt -1.0) : self.zoom = (-1.0D / zoom)
            (zoom gt 1.0) : self.zoom = zoom
            else : self.zoom = 1.0D
        endcase

        viewplaneRect = dblarr(4, /NOZERO)
        viewplaneRect[0:1] = self.viewcoord[0:1] - $
                (self.viewRect[2:3] / (2.0D * self.zoom))
        viewplaneRect[2:3] = self.viewRect[2:3] / self.zoom

        self -> IDLgrView::SetProperty, VIEWPLANE_RECT= viewplaneRect
    endif

end


pro Camera::Roll,   droll, $
                    no_transform=noTransform

    ;  Roll the camera about the z axis.

    compile_opt idl2

    if (N_ELEMENTS(droll) eq 1) then begin
        self.roll = self.roll + droll
        if (self.roll gt 359) then self.roll = self.roll - 360.
        if (self.roll lt 0) then self.roll = self.roll + 360.

        self.rotQuat -> Set, 0.0, 0.0, droll

        self.Orientation -> PostMult, self.rotQuat -> GetQuat()

        if (not KEYWORD_SET(noTransform)) then self -> Transform
    endif

end


pro Camera::Pan,    dyaw, $
                    dpitch, $
                    no_transform=noTransform

    ;  Pan the camera, rotating about the x and y axes.

    compile_opt idl2

    if (N_ELEMENTS(dyaw) eq 1) and (N_ELEMENTS(dpitch) eq 1) then begin
        self.pitch = self.pitch + dpitch
        if (self.pitch gt 359) then self.pitch = self.pitch - 360.
        if (self.pitch lt 0) then self.pitch = self.pitch + 360.
        self.yaw = self.yaw + dyaw
        if (self.yaw gt 359) then self.yaw = self.yaw - 360.
        if (self.yaw lt 0) then self.yaw = self.yaw + 360.

        if (self.lock) then begin
            self.Orientation -> Set, self.pitch, self.yaw, self.roll
        endif else begin
            self.rotQuat -> Set, dpitch, dyaw, 0.0
            self.Orientation -> PostMult, self.rotQuat -> GetQuat()
        endelse

        if (not KEYWORD_SET(noTransform)) then self -> Transform
    endif

end


pro Camera::Truck,  truckAxis, $
                    distance, $
                    no_transform=noTransform

    ;  "Truck" (move) the camera relative to its current orientation.

    compile_opt idl2

    if (N_ELEMENTS(truckAxis) eq 3) and (N_ELEMENTS(distance) eq 1) then begin

        v = TOTAL(truckAxis^2)
        if ( v eq 0.) then RETURN
        truckAxis = TEMPORARY(truckAxis) / SQRT(v)

        yaw = -ATAN(truckAxis[0],truckAxis[2]) * !RADEG
        pitch = ATAN(truckAxis[1], SQRT(truckAxis[2]^2 + truckAxis[0]^2)) * $
            !RADEG

        self.rotQuat -> Set, pitch, yaw, 0.0
        self.truckQuat -> SetQuat, self.Orientation -> GetQuat()

        self.truckQuat -> PostMult, self.rotQuat -> GetQuat()

        ovect = self.truckQuat -> GetDirectionVector()
        self.cameraLocation = self.cameraLocation + (ovect * distance)

        if (not KEYWORD_SET(noTransform)) then self -> Transform
    endif

end


pro Camera::SetProperty,    camera_location=cameraLocation, $
                            lock=lock, $
                            lookat=lookat, $
                            no_transform=noTransform, $
                            pitch=pitch, $
                            roll=roll, $
                            third_person=thirdPerson, $
                            track=track, $
                            viewplane_rect=viewplaneRect, $
                            yaw=yaw, $
                            zclip=zclip, $
                            _extra=extra

    compile_opt idl2

    updateViewcoord = 0B
    updateOrientation = 0B

    if (N_ELEMENTS(cameraLocation) eq 3) then self.cameraLocation = $
        cameraLocation
    if (N_ELEMENTS(lock) ne 0) then self.lock = KEYWORD_SET(lock)
    if (N_ELEMENTS(lookat) eq 3) then self.lookat = lookat
    if (N_ELEMENTS(pitch) eq 1) then begin
        self.pitch = pitch
        updateOrientation = 1B
    endif
    if (N_ELEMENTS(roll) eq 1) then begin
        self.roll = roll
        updateOrientation = 1B
    endif
    if (N_ELEMENTS(thirdPerson) eq 1) then begin
        self.thirdPerson = thirdPerson
        updateViewcoord = 1B
    endif
    if (N_ELEMENTS(track) ne 0) then self.track = KEYWORD_SET(track)
    if (N_ELEMENTS(yaw) eq 1) then begin
        self.yaw = yaw
        updateOrientation = 1B
    endif
    if (N_ELEMENTS(viewplaneRect) eq 4) then begin
        self.viewRect = viewplaneRect
        self -> IDLgrView::SetProperty, VIEWPLANE_RECT= $
            (viewplanerect / self.zoom)
        update_viewcoord = 1B
    endif

    self -> IDLgrView::SetProperty, _EXTRA=extra

    if (N_ELEMENTS(zclip) ne 0) then begin
        self -> IDLgrView::SetProperty, ZCLIP=zclip
        updateViewcoord = 1B
    end

    if (updateOrientation) then $
        self.orientation -> Set, self.pitch, self.yaw, self.roll

    if (updateViewcoord) then begin
        self -> IDLgrView::GetProperty, ZCLIP=zclip
        self.viewcoord[0] = ((2. * self.viewRect[0]) + self.viewRect[2]) / 2.
        self.viewcoord[1] = ((2. * self.viewRect[1]) + self.viewRect[3]) / 2.
        self.viewcoord[2] = zclip[0] - self.thirdPerson
    endif

    if (not KEYWORD_SET(noTransform)) then self -> Transform

end


pro Camera::GetProperty,    camera_location=cameraLocation, $
                            lock=lock, $
                            lookat=lookat, $
                            pitch=pitch, $
                            quaternion=quaternion, $
                            roll=roll, $
                            third_person=thirdPerson, $
                            track=track, $
                            ;viewplane_rect=viewplaneRect, $
                            yaw=yaw, $
                            zoom=zoom, $
                            _ref_extra=extra


    compile_opt idl2

    cameraLocation = self.cameraLocation
    lock = self.lock
    lookat = self.lookat
    pitch = self.pitch
    quaternion = self.orientation
    roll = self.roll
    thirdPerson = self.thirdPerson
    track = self.track
    ;viewplaneRect = self.viewRect
    yaw = self.yaw
    zoom = self.zoom

    self -> IDLgrView::GetProperty, _EXTRA=extra

end


function Camera::GetDirectionVector

    compile_opt idl2

    RETURN, self.orientation -> GetDirectionVector()

end


pro Camera::Cleanup

    compile_opt idl2

    OBJ_DESTROY, [self.orientation, self.rotQuat, self.truckQuat]

    self -> IDLgrView::Cleanup

end


pro Camera__Define

    struct={Camera, $
            inherits IDLgrView, $

            cameraLocation:FLTARR(3), $
            lock:0, $
            lookat:fltarr(3), $
            orientation:OBJ_NEW(), $
            pitch:0., $
            roll:0., $
            rotQuat:OBJ_NEW(), $
            thirdPerson:0., $
            track:0, $
            truckQuat:OBJ_NEW(), $
            viewcoord:FLTARR(3), $
            viewRect:DBLARR(4), $
            yaw:0., $
            zoom:0D}

end
