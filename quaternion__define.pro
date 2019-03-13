;+
; NAME:
;       QUATERNION__DEFINE
;
; PURPOSE:
;       The purpose of this routine is to provide an easy mechanism
;       to store and manipulate model orientations.  Quaternions
;       do not suffer from the limitations of the eular angle system.
;
;
; AUTHOR:
;       Rick Towler
;       School of Aquatic and Fishery Sciences
;       University of Washington
;       Box 355020
;       Seattle, WA 98195-5020
;       rtowler@u.washington.edu
;       http://www.acoustics.washington.edu
;
;
; CATEGORY:
;       Object Graphics
;
;
; CALLING SEQUENCE:
;       quat = Obj_New("quaternion" [, PITCH{Get, Set}=value{0 to 360}]
;               [, YAW{Get, Set}=value{0 to 360}] [, ROLL{Get, Set}=value{0 to 360}])
;
;
; KEYWORD PARAMETERS:
;
;   pitch:          A scalar defining the initial quaternion orientation about the
;                   X axis in degrees.
;
;                   Default: 0.
;
;   yaw:            A scalar defining the initial quaternion orientation about the
;                   Y axis in degrees.
;
;                   Default: 0.
;
;   roll:           A scalar defining the initial quaternion orientation about the
;                   Z axis in degrees.
;
;                   Default: 0.
;
;
; OBJECT METHODS:
;
;   GetCTM:         This function method returns a 4x4 transformation matrix
;                   representing the quaternion orientation.
;
;                   matrix = fltarr(4,4)
;                   matrix = quat -> GetCTM()
;
;
;   GetDirectionVector: This function method returns a unit vector representing
;                       the object's current orientation as a 3 element float
;                       [X,Y,Z]
;
;                       vector = fltarr(3)
;                       vector = quaternion -> GetDirectionVector
;
;
;   GetPYR:         This function method returns the object's current orientation
;                   as a 3 element vector [pitch,yaw,roll] (as defined in the
;                   keywords section above).
;
;                   euler_orientation = quaternion -> GetPYR()
;
;
;   GetQuat:        This function method returns the object's value
;                   as a 4 element float.  The values returned are in the form
;                   [w,x,y,z]
;
;                   q = dblarr(4)
;                   q = quaternion -> GetQuat()
;
;
;   Interpolate:    This function method returns a quaternion in the form
;                   [w,x,y,z] which represents a spherically interpolated
;                   orientation based on the object's value, a "to" value,
;                   and a blend value ranging from 0.0 to 1.0.  A blend of
;                   0.0 will return the object's value and a blend of 1.0
;                   will return the "to" value.
;
;                   quat_one = obj_new('quaternion', pitch=0., yaw=0.)
;                   quat_two = obj_new('quaternion', pitch=50., yaw=270.)
;
;                   ;get a quaternion with orientation halfway between
;                   ;quat_one and quat_two
;                   q = dblarr(4)
;                   q = quat_one -> Interpolate, quat_two -> GetQuat(), 0.5
;
;
;   Reset:          Reset the quaternion to the initial orientation.
;
;                   quaternion -> Reset
;
;
;   Set:            This procedure method will set the current orientation to the
;                   provided pitch, yaw, and roll values.
;
;                   quaternion -> Set, 15., 90., 0.
;
;
;   SetQuat:        This procedure method sets the object's value to the
;                   given value.  The value must be in the form:
;                   [w,x,y,z]
;
;                   quat_one = obj_new('quaternion')
;                   quat_two = obj_new('quaternion')
;
;                   <do something to quat_one>
;
;                   ;set quat_two equal to quat_one
;                   quat_two -> SetQuat, quat_one -> GetQuat()
;
;
; EXAMPLE:
;
;                   Take a look at camera__define.pro for a good example of
;                   this object's uses.
;
;
; MODIFICATION HISTORY:
;       Written by: Rick Towler, University of Washington
;                   01 November 2000.
;              RHT: 29 March 2002 - Added getPYR procedure.
;
;
; LICENSE
;
;   QUATERNION__DEFINE.PRO Copyright (C) 2001  Rick Towler
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
;   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;   A full copy of the GNU General Public License can be found on line at
;   http://www.gnu.org/copyleft/gpl.html#SEC1
;
;-


Function Quaternion::init,      pitch=pitch, $
                                yaw=yaw, $
                                roll=roll

    compile_opt idl2

    if (n_elements(pitch) eq 0) then pitch = 0.
    if (n_elements(yaw) eq 0) then yaw = 0.
    if (n_elements(roll) eq 0) then roll = 0.

    self -> Set,  pitch, yaw, roll

    self.initial = self -> GetQuat()

    return, 1

End


Pro Quaternion::Set, x, y, z
    ;set the rotation angles (in degrees) around the x, y and z axes
    ;invert the x & y values so our rotations are right handed.

    compile_opt idl2

    if (n_elements(x) eq 1) and (n_elements(y) eq 1) and $
            (n_elements(z) eq 1) then begin
        xQ = self -> AxisAngle2Quat (-x, 1.0, 0.0, 0.0)
        yQ = self -> AxisAngle2Quat (-y, 0.0, 1.0, 0.0)
        zQ = self -> AxisAngle2Quat (z, 0.0, 0.0, 1.0)

        self.q = yQ

        self -> PostMult, xQ
        self -> PostMult, zQ

        self -> Normalize
    endif

End


Function Quaternion::GetPYR

    compile_opt idl2

    transform = self -> getCTM()

    yaw =  -asin(transform[2])
    ;yaw = asin(transform[2])
    c =  cos(yaw); * !RADEG
    yaw = temporary(yaw) * !RADEG

    if (abs(yaw) > 0.005D) then begin

        trx =  transform[10] / c
        try = -transform[6]  / c

        pitch  = -atan(try,trx) * !RADEG
        ;pitch  = atan(try,trx) * !RADEG

        trx = transform[0] / c
        try = -transform[1] / c

        roll = atan(try,trx) * !RADEG

    endif else begin

      pitch = 0D

      trx = transform[5]
      try = transform[4]

      roll = atan(try,trx) * !RADEG

    endelse

    if (pitch lt 0D) then pitch = pitch + 360D
    if (yaw lt 0D) then yaw = yaw + 360D
    if (roll lt 0D) then roll = roll + 360D

    return, [pitch,yaw,roll]

end


Function Quaternion::AxisAngle2Quat, angle, x, y, z
    ;return a quaternion based on the angle and axis of rotation

    compile_opt idl2

    q = dblarr(4)
    angle = angle * !DTOR

    scale = sqrt(x^2 + y^2 + z^2)
    x = x / scale
    y = y / scale
    z = z / scale

    q[0] = cos(angle / 2.0D)
    sinHalfAngle = sin(angle / 2.0D)
    q[1] = x * sinHalfAngle
    q[2] = y * sinHalfAngle
    q[3] = z * sinHalfAngle

    return, q
End


Function Quaternion::GetCTM
    ;return a 4x4 transformation matrix representing
    ;our quaternion's orientation

    compile_opt idl2

    matrix = dblarr(4,4)

    w = self.q[0]
    x = self.q[1]
    y = self.q[2]
    z = self.q[3]

    x2 = x + x
    y2 = y + y
    z2 = z + z
    xx = x * x2
    xy = x * y2
    xz = x * z2
    yy = y * y2
    yz = y * z2
    zz = z * z2
    wx = w * x2
    wy = w * y2
    wz = w * z2

    matrix[0,0] = 1.0D - (yy + zz)
    matrix[1,0] = xy - wz
    matrix[2,0] = xz + wy

    matrix[0,1] = xy + wz
    matrix[1,1] = 1.0D - (xx + zz)
    matrix[2,1] = yz - wx

    matrix[0,2] = xz - wy
    matrix[1,2] = yz + wx
    matrix[2,2] = 1.0D - (xx + yy)

    matrix[3,3] = 1.0D

    return, matrix
End


Pro Quaternion::Reset
    ;reset to the initial orientation

    compile_opt idl2

    self.q = self.initial
End


Function Quaternion::GetQuat
    ;return the 4 element quaternion

    compile_opt idl2

    return, self.q
End


Pro Quaternion::SetQuat, q
    ;set the 4 element quaternion

    compile_opt idl2

    if (n_elements(q) eq 4) then begin
        self.q = double(q)
        self -> normalize
    endif

End


Pro Quaternion::PostMult, quat

    compile_opt idl2

    if (n_elements(quat) eq 4) then self -> MultAndSet, self.q, quat

End


Pro Quaternion::MultAndSet, quat1, quat2

    compile_opt idl2

    self.q[0] = quat2[0] * quat1[0] - $
                quat2[1] * quat1[1] - $
                quat2[2] * quat1[2] - $
                quat2[3] * quat1[3]

    self.q[1] = quat2[0] * quat1[1] + $
                quat2[1] * quat1[0] + $
                quat2[2] * quat1[3] - $
                quat2[3] * quat1[2]

    self.q[2] = quat2[0] * quat1[2] - $
                quat2[1] * quat1[3] + $
                quat2[2] * quat1[0] + $
                quat2[3] * quat1[1]

    self.q[3] = quat2[0] * quat1[3] + $
                quat2[1] * quat1[2] - $
                quat2[2] * quat1[1] + $
                quat2[3] * quat1[0]

    self -> normalize
End


Function Quaternion::GetDirectionVector
    ;return a unit vector representing the current orientation

    compile_opt idl2

    w = self.q[0]
    x = self.q[1]
    y = self.q[2]
    z = self.q[3]

    dirX = -2.0 * (x * z - w * y)
    dirY = -2.0 * (y * z + w * x)
    dirZ = -1.0 + 2.0 * (x * x + y * y)

    return, [dirX, dirY, dirZ]
End


Pro Quaternion::Normalize

    compile_opt idl2

    scale = sqrt(total(self.q^2))
    self.q = self.q / scale
End


Function Quaternion::Interpolate, qTo, blend
    ;interpolate from the object's current orientation (self.q) to
    ;a new orientation, qTo, using spherical linear interpolation.
    ;Adapted from: Watt and Watt, Advanced Animation p. 364

    compile_opt idl2

    if (n_elements(qTo) eq 4) and (n_elements(blend) eq 1) then begin

        blend = 0.0 > blend < 1.0
        delta = 0.001
        qResult = dblarr(4)

        cosom = total(self.q * qTo)

        if (cosom lt 0) then begin
            cosom = -cosom
            self.q = -self.q
        endif

        if ((1.0 - cosom) gt delta) then begin
            omega = acos(cosom)
            sinom = sin(omega)
            scale0 = sin((1.0 - blend) * omega) / sinom
            scale1 = sin(blend * omega) / sinom
        endif else begin
            scale0 = 1.0 - blend
            scale1 = blend
        endelse

        qResult = (scale0 * self.q) + (scale1 * qTo)

        return, qResult
    endif

End


Function Quaternion::Quat2AxisAngle
    ;return the 4 element axis  and values of the current orientation

    compile_opt idl2

    axisangle = dblarr(4)

    tw = acos(self.q[0]) * 2.0
    scale = sin(tw / 2.0)

    axisangle = self.q / scale

    axisangle[0] = (tw * 180.0) / !PI

    return, axisangle
End


Pro Quaternion::cleanup

    compile_opt idl2

End


Pro Quaternion__define

    struct={Quaternion, $
            q:dblarr(4), $
            initial:dblarr(4)}
End