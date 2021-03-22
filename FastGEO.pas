(*************************************************************************)
(*                                                                       *)
(*                             FASTGEO                                   *)
(*                                                                       *)
(*                2D/3D Computational Geometry Algorithms                *)
(*                        Release Version 5.0.1                          *)
(*                                                                       *)
(* Author: Arash Partow 1997-2007                                        *)
(* URL: http://fastgeo.partow.net                                        *)
(*      http://www.partow.net/projects/fastgeo/index.html                *)
(*                                                                       *)
(* Copyright notice:                                                     *)
(* Free use of the FastGEO computational geometry library is permitted   *)
(* under the guidelines and in accordance with the most current version  *)
(* of the Common Public License.                                         *)
(* http://www.opensource.org/licenses/cpl.php                            *)
(*                                                                       *)
(* Modified by JP CASSOU                                                 *)
(*************************************************************************)


unit FastGEO;

interface


const VersionInformation = 'FastGEO Version 5.0.1';
const AuthorInformation  = 'Arash Partow (1997-2007)';
const EpochInformation   = 'Lambda-Phi';
const RecentUpdate       = '07-07-2007';
const LCID               = '$07-07-2007:FEDEDB4632780C2FAE$';


{$IFNDEF FASTGEO}
  {$DEFINE FASTGEO}
{$ENDIF}


{$DEFINE FASTGEO_DOUBLE_PRECISION}


{$IFDEF FASTGEO_SINGLE_PRECISION}
 type TGeoFloat = Single;
{$ENDIF}

{$IFDEF FASTGEO_DOUBLE_PRECISION}
 type TGeoFloat = Double;
{$ENDIF}

{$IFDEF FASTGEO_EXTENDED_PRECISION}
 type TGeoFloat = Extended;
{$ENDIF}




(****************************************************************************)
(********************[ Basic Geometric Structure Types ]*********************)
(****************************************************************************)

(**************[  Vertex type   ]***************)
type TGeoPoint2DPtr    = ^TGeoPoint2D;
     TGeoPoint2D       = record x,y:TGeoFloat; end;
     TGeoPoint2DArray  = array of TGeoPoint2D;


(**************[ 3D Vertex type ]***************)
type TGeoPoint3DPtr    = ^TGeoPoint3D;
     TGeoPoint3D       = record x,y,z:TGeoFloat; end;
     TGeoPoint3DArray  = array of TGeoPoint3D;


(**************[  Quadix type   ]***************)
type TQuadix2DPtr   = ^TQuadix2D;
     TQuadix2D      = array [1..4] of TGeoPoint2D;
     TQuadix2DArray = array of TQuadix2D;

type TQuadix3DPtr   = ^TQuadix3D;
     TQuadix3D      = array [1..4] of TGeoPoint3D;
     TQuadix3DArray = array of TQuadix3D;


(**************[ Rectangle type ]***************)
type TGeoRectanglePtr   = ^TGeoRectangle;
     TGeoRectangle      = array [1..2] of TGeoPoint2D;
     TGeoRectangleArray = array of TGeoRectangle;


(**************[   Box type     ]***************)
type TGeoBoxPtr   = ^TGeoRectangle;
     TGeoBox      = array [1..2] of TGeoPoint3D;
     TGeoBoxArray = array of TGeoBox;


(**************[ Triangle type  ]***************)
type TGeoTriangle2DPtr   = ^TGeoTriangle2D;
     TGeoTriangle2D      = array [1..3] of TGeoPoint2D;
type TGeoTriangle2DArray = array of TGeoTriangle2D;

type TGeoTriangle3DPtr   = ^TGeoTriangle3D;
     TGeoTriangle3D      = array [1..3] of TGeoPoint3D;

type TGeoTriangle3DArray = array of TGeoTriangle3D;


(**************[  Segment type  ]***************)
type TGeoSegment2DPtr   = ^TGeoSegment2D;
     TGeoSegment2D      = array [1..2] of TGeoPoint2D;
     TGeoSegment2DArray = array of TGeoSegment2D;

type TGeoSegment3DPtr   = ^TGeoSegment3D;
     TGeoSegment3D      = array [1..2] of TGeoPoint3D;
     TGeoSegment3DArray = array of TGeoSegment3D;


(**************[  Line type  ]***************)
type TGeoLine2DPtr     = ^TGeoLine2D;
     TGeoLine2D        = array [1..2] of TGeoPoint2D;
     TGeoLine2DArray   = array of TGeoLine2D;

type TGeoLine3DPtr     = ^TGeoLine3D;
     TGeoLine3D        = array [1..2] of TGeoPoint3D;
     TGeoLine3DArray   = array of TGeoLine3D;


(**************[  Circle type   ]***************)
type TGeoCirclePtr     = ^TGeoCircle;
     TGeoCircle        = record x,y,Radius : TGeoFloat; end;
     TGeoCircleArray   = array of TGeoCircle;


(**************[  Sphere type   ]***************)
type TGeoSpherePtr     = ^TGeoSphere;
     TGeoSphere        = record x,y,z,Radius : TGeoFloat; end;
     TGeoSphereArray   = array of TGeoSphere;


(**************[  Arc type   ]***************)
type TGeoCircularArc2DPtr = ^TGeoCircularArc2D;
     TGeoCircularArc2D    = record
                           x1,y1,x2,y2,cx,cy,px,py : TGeoFloat;
                           angle1, angle2          : TGeoFloat;
                           Orientation             : Integer;
                         end;
     TGeoCircularArc2DArray = array of TGeoCircularArc2D;


(************[  Bezier type   ]*************)
type TGeoQuadraticBezier2DPtr   = ^TGeoQuadraticBezier2D;
     TGeoQuadraticBezier2D      = array [0..2] of TGeoPoint2D;
     TGeoQuadraticBezier2DArray = array of TGeoQuadraticBezier2D;

     TGeoQuadraticBezier3DPtr   = ^TGeoQuadraticBezier3D;
     TGeoQuadraticBezier3D      = array [0..2] of TGeoPoint3D;
     TGeoQuadraticBezier3DArray = array of TGeoQuadraticBezier3D;

     TGeoCubicBezier2DPtr       = ^TGeoCubicBezier2D;
     TGeoCubicBezier2D          = array [0..3] of TGeoPoint2D;
     TGeoCubicBezier2DArray     = array of TGeoCubicBezier2D;

     TGeoCubicBezier3DPtr       = ^TGeoCubicBezier3D;
     TGeoCubicBezier3D          = array [0..3] of TGeoPoint3D;
     TGeoCubicBezier3DArray     = array of TGeoCubicBezier3D;

     TGeoCurvePoint2D           = record x,y,t   : TGeoFloat; end;
     TGeoCurvePoint3D           = record x,y,z,t : TGeoFloat; end;

     TGeoCurvePoint2DArray      = array of TGeoCurvePoint2D;
     TGeoCurvePoint3DArray      = array of TGeoCurvePoint3D;


(**************[ 2D Vector type ]***************)
type TGeoVector2D      = record x,y : TGeoFloat; end;
type TGeoVector2DPtr   = ^TGeoVector2D;
type TGeoVector2DArray = array of TGeoVector2D;


(**************[ 3D Vector type ]***************)
type TGeoVector3D      = record x,y,z : TGeoFloat; end;
type TGeoVector3DPtr   = ^TGeoVector3D;
type TGeoVector3DArray = array of TGeoVector3D;


(**********[ Polygon Vertex type  ]************)
type TGeoPolygon2D     = array of TGeoPoint2D;
type TGeoPolygon2DPtr  = ^TGeoPolygon2D;

type TGeoPolyLine2D     = array of TGeoPoint2D;
type TGeoPolyLine2DPtr  = ^TGeoPolyLine2D;

type TGeoPolygon3D     = array of TGeoPoint3D;
type TGeoPolygon3DPtr  = ^TGeoPolygon3D;

type TGeoPolyhedron    = array of TGeoPolygon3D;
type TGeoPolyhedronPtr = ^TGeoPolyhedron;


(**************[ Plane type ]******************)

type TGeoPlane2D       = record a,b,c,d : TGeoFloat; end;
type TGeoPlane2DPtr    = ^TGeoPlane2D;


(**********[ Barycentric Coordinates]***********)

type TBarycentricUnit       = record x1,y1,x2,y2,x3,y3,delta : TGeoFloat; end;
type TBarycentricUnitPtr    = ^TBarycentricUnit;

type TBarycentricTriplet    = record u,v,w : TGeoFloat; end;
type TBarycentricTripletPtr = ^TBarycentricTriplet;


type TInclusion    = (
                      eFully,
                      ePartially,
                      eOutside,
                      eUnknown
                     );

type eTriangletype = (
                      etEquilateral,
                      etIsosceles,
                      etRight,
                      etScalene,
                      etObtuse,
                      etUnknown
                     );



(********[ Universal Geometric Variable ]********)

type eGeometricObjectTypes = (
                              geoPoint2D,
                              geoPoint3D,
                              geoLine2D,
                              geoLine3D,
                              geoSegment2D,
                              geoSegment3D,
                              geoQuadix2D,
                              geoQuadix3D,
                              geoTriangle2D,
                              geoTriangle3D,
                              geoRectangle,
                              geoCircle,
                              geoSphere,
                              geoPolygon2D,
                              geoPolygon3D,
                              geoQuadraticBezier2D,
                              geoQuadraticBezier3D,
                              geoCubicBezier2D,
                              geoCubicBezier3D,
                              geoPolyhedron
                             );



type TGeometricObjectPtr = ^TGeometricObject;
     TGeometricObject = record
       case ObjectType : eGeometricObjectTypes of
         geoPoint2D           : (Point2D    : TGeoPoint2D          );
         geoPoint3D           : (Point3D    : TGeoPoint3D          );
         geoLine2D            : (Line2D     : TGeoLine2D           );
         geoLine3D            : (Line3D     : TGeoLine3D           );
         geoSegment2D         : (Segment2D  : TGeoSegment2D        );
         geoSegment3D         : (Segment3D  : TGeoSegment3D        );
         geoTriangle2D        : (Triangle2D : TGeoTriangle2D       );
         geoTriangle3D        : (Triangle3D : TGeoTriangle3D       );
         geoQuadix2D          : (Quadix2D   : TQuadix2D         );
         geoQuadix3D          : (Quadix3D   : TQuadix3D         );
         geoRectangle         : (Rectangle  : TGeoRectangle        );
         geoCircle            : (Circle     : TGeoCircle           );
         geoSphere            : (Sphere     : TGeoSphere           );
         geoPolygon2D         : (Polygon2D  : TGeoPolygon2DPtr     );
         geoPolygon3D         : (Polygon3D  : TGeoPolygon3DPtr     );
         geoQuadraticBezier2D : (QBezier2D  : TGeoQuadraticBezier2D);
         geoQuadraticBezier3D : (QBezier3D  : TGeoQuadraticBezier3D);
         geoCubicBezier2D     : (CBezier2D  : TGeoCubicBezier2D    );
         geoCubicBezier3D     : (CBezier3D  : TGeoCubicBezier3D    );
         geoPolyhedron        : (Polyhedron : TGeoPolyhedronPtr    );
       end;



type TGeoBooleanArray = array of Boolean;


type TNumericPrecisionResult = record
       EEResult      : Boolean;  // Epsilon equivelence result
       ZEResult      : Boolean;  // Zero equivelence result;
       EFPResult     : Boolean;  // Extended floating point test result
       SystemEpsilon : TGeoFloat;
end;


(**********[ Orientation constants ]**********)

const geoRightHandSide        = -1;
const geoLeftHandSide         = +1;
const geoClockwise            = -1;
const geoCounterClockwise     = +1;
const geoCollinearOrientation =  0;
const geoAboveOrientation     = +1;
const geoBelowOrientation     = -1;
const geoCoplanarOrientation  =  0;



(************[ Epsilon constants ]*************)

const Epsilon_High      = 1.0E-16;
const Epsilon_Medium    = 1.0E-12;
const Epsilon_Low       = 1.0E-08;
const Epsilon           = Epsilon_Medium;
const Zero              = 0.0;

{$IFDEF FASTGEO_SINGLE_PRECISION}
 const Infinity          = 1e+30;
{$ENDIF}

{$IFDEF FASTGEO_DOUBLE_PRECISION}
 const Infinity          = 1e+300;
{$ENDIF}

{$IFDEF FASTGEO_EXTENDED_PRECISION}
 const Infinity          = 1e+4924;
{$ENDIF}


type TPointList2D = array of TGeoPoint2D;
type TPointList3D = array of TGeoPoint3D;


(*******[ Random resolution constants ]********)

const RandomResolutionInt = 1000000000;
const RandomResolutionFlt = RandomResolutionInt * 1.0;



function Orientation(const x1,y1,x2,y2,Px,Py:TGeoFloat):Integer;                                                                  overload;
function Orientation(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TGeoFloat):Integer;                                                overload;

function RobustOrientation(const x1,y1,x2,y2,Px,Py:TGeoFloat):Integer;                                                            overload;
function RobustOrientation(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TGeoFloat):Integer;                                          overload;

function Orientation(const Point1,Point2:TGeoPoint2D; const Px,Py:TGeoFloat):Integer;                                                overload;
function Orientation(const Point1,Point2,Point3:TGeoPoint2D):Integer;                                                             overload;
function Orientation(const Line:TGeoLine2D; const Point:TGeoPoint2D):Integer;                                                        overload;
function Orientation(const Segment:TGeoSegment2D; const Point:TGeoPoint2D):Integer;                                                  overload;

function Orientation(const Point1,Point2,Point3:TGeoPoint3D; const Px,Py,Pz:TGeoFloat):Integer;                                      overload;
function Orientation(const Point1,Point2,Point3,Point4:TGeoPoint3D):Integer;                                                      overload;
function Orientation(const Triangle:TGeoTriangle3D; const Point:TGeoPoint3D):Integer;                                                overload;

function Signed(const x1,y1,x2,y2,Px,Py:TGeoFloat):TGeoFloat;                                                                        overload;
function Signed(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TGeoFloat):TGeoFloat;                                                      overload;

function Signed(const Point1,Point2:TGeoPoint2D; const Px,Py:TGeoFloat):TGeoFloat;                                                      overload;
function Signed(const Point1,Point2,Point3:TGeoPoint2D):TGeoFloat;                                                                   overload;
function Signed(const Line:TGeoLine2D; const Point:TGeoPoint2D):TGeoFloat;                                                              overload;
function Signed(const Segment:TGeoSegment2D; const Point:TGeoPoint2D):TGeoFloat;                                                        overload;

function Signed(const Point1,Point2,Point3:TGeoPoint3D; const Px,Py,Pz:TGeoFloat):TGeoFloat;                                            overload;
function Signed(const Point1,Point2,Point3,Point4:TGeoPoint3D):TGeoFloat;                                                            overload;
function Signed(const Triangle:TGeoTriangle3D; const Point:TGeoPoint3D):TGeoFloat;                                                      overload;

function Collinear(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;                                                                    overload;
function Collinear(const x1,y1,x2,y2,x3,y3,Epsilon:TGeoFloat):Boolean;                                                            overload;
function Collinear(const PointA,PointB,PointC:TGeoPoint2D):Boolean;                                                               overload;
function Collinear(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;                                                           overload;
function Collinear(const PointA,PointB,PointC:TGeoPoint3D):Boolean;                                                               overload;

function RobustCollinear(const x1,y1,x2,y2,x3,y3:TGeoFloat; const Epsilon : TGeoFloat = Epsilon_High):Boolean;                       overload;
function RobustCollinear(const PointA,PointB,PointC:TGeoPoint2D;  const Epsilon : TGeoFloat = Epsilon_High):Boolean;                 overload;

function RobustCollinear(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat; const Epsilon : TGeoFloat = Epsilon_High):Boolean;              overload;
function RobustCollinear(const PointA,PointB,PointC:TGeoPoint3D;  const Epsilon : TGeoFloat = Epsilon_High):Boolean;                 overload;

function Coplanar(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat):Boolean;                                                   overload;
function Coplanar(const PointA,PointB,PointC,PointD:TGeoPoint3D):Boolean;                                                         overload;

function IsPointCollinear(const x1,y1,x2,y2,Px,Py:TGeoFloat;                   const Robust : Boolean = False):Boolean;           overload;
function IsPointCollinear(const PointA,PointB,PointC:TGeoPoint2D;              const Robust : Boolean = False):Boolean;           overload;
function IsPointCollinear(const PointA,PointB:TGeoPoint2D; const Px,Py:TGeoFloat; const Robust : Boolean = False):Boolean;           overload;
function IsPointCollinear(const Segment:TGeoSegment2D; const PointC:TGeoPoint2D;  const Robust : Boolean = False):Boolean;           overload;
function IsPointCollinear(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TGeoFloat):Boolean;                                                    overload;
function IsPointCollinear(const PointA,PointB,PointC:TGeoPoint3D):Boolean;                                                        overload;
function IsPointCollinear(const Segment:TGeoSegment3D; const PointC:TGeoPoint3D):Boolean;                                            overload;

function IsPointOnRightSide(const Px,Py,x1,y1,x2,y2:TGeoFloat):Boolean;                                                           overload;
function IsPointOnRightSide(const x,y:TGeoFloat; const Segment:TGeoSegment2D):Boolean;                                               overload;
function IsPointOnRightSide(const Point:TGeoPoint2D; const Segment:TGeoSegment2D):Boolean;                                           overload;

function IsPointOnRightSide(const x,y:TGeoFloat; const Line:TGeoLine2D):Boolean;                                                     overload;
function IsPointOnRightSide(const Point:TGeoPoint2D; const Line:TGeoLine2D):Boolean;                                                 overload;

function IsPointOnLeftSide(const Px,Py,x1,y1,x2,y2:TGeoFloat):Boolean;                                                            overload;
function IsPointOnLeftSide(const x,y:TGeoFloat; const Segment:TGeoSegment2D):Boolean;                                                overload;
function IsPointOnLeftSide(const Point:TGeoPoint2D; const Segment:TGeoSegment2D):Boolean;                                            overload;

function IsPointOnLeftSide(const x,y:TGeoFloat; const Line:TGeoLine2D):Boolean;                                                      overload;
function IsPointOnLeftSide(const Point:TGeoPoint2D; const Line:TGeoLine2D):Boolean;                                                  overload;

function Intersect(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;                                                              overload;
function Intersect(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; out ix, iy :TGeoFloat):Boolean;                                          overload;
function Intersect(const Point1,Point2,Point3,Point4:TGeoPoint2D):Boolean;                                                        overload;
function Intersect(const Segment1,Segment2:TGeoSegment2D):Boolean;                                                                overload;
function Intersect(const Segment1,Segment2:TGeoSegment2D; out ix, iy : TGeoFloat):Boolean;                                           overload;

function Intersect(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; const fuzzy:TGeoFloat = Zero):Boolean;                       overload;
function Intersect(const P1,P2,P3,P4:TGeoPoint3D; const fuzzy:TGeoFloat = Zero):Boolean;                                             overload;
function Intersect(const Segment1,Segment2:TGeoSegment3D; const fuzzy:TGeoFloat = Zero):Boolean;                                     overload;

function Intersect(const Segment:TGeoSegment2D;   const Rectangle : TGeoRectangle):Boolean;                                          overload;
function Intersect(const Segment:TGeoSegment2D;   const Triangle  : TGeoTriangle2D):Boolean;                                         overload;
function Intersect(const Segment:TGeoSegment2D;   const Quadix    : TQuadix2D):Boolean;                                           overload;
function Intersect(const Segment:TGeoSegment2D;   const Line      : TGeoLine2D):Boolean;                                             overload;
function Intersect(const Segment:TGeoSegment2D;   const Circle    : TGeoCircle):Boolean;                                             overload;
function Intersect(const Segment:TGeoSegment3D;   const Sphere    : TGeoSphere):Boolean;                                             overload;
function Intersect(const Line:TGeoLine2D;         const Triangle  : TGeoTriangle2D):Boolean;                                         overload;
function Intersect(const Line:TGeoLine2D;         const Quadix    : TQuadix2D):Boolean;                                           overload;
function Intersect(const Line:TGeoLine2D;         const Circle    : TGeoCircle):Boolean;                                             overload;
// intersection droite 3D et triangle, retourne les coordonnées de l'intersection si elle existe
function Intersect(const Line:TGeoLine3D;         const Triangle  : TGeoTriangle3D; out IPoint: TGeoPoint3D):Boolean;                   overload;
function Intersect(const Triangle:TGeoTriangle2D; const Circle    : TGeoCircle):Boolean;                                             overload;
function Intersect(const Triangle:TGeoTriangle2D; const Rectangle : TGeoRectangle):Boolean;                                          overload;
function Intersect(const Rectangle1,Rectangle2:TGeoRectangle):Boolean;                                                            overload;
function Intersect(const Triangle1,Triangle2:TGeoTriangle2D):Boolean;                                                             overload;
function Intersect(const Rectangle:TGeoRectangle; const Circle:TGeoCircle):Boolean;                                                  overload;
function Intersect(const Circle1,Circle2:TGeoCircle):Boolean;                                                                     overload;
function Intersect(const Sphere1,Sphere2:TGeoSphere):Boolean;                                                                     overload;
function Intersect(const Poly1,Poly2:TGeoPolygon2D):Boolean;                                                                      overload;
function Intersect(const Obj1,Obj2:TGeometricObject):Boolean;                                                                  overload;

function SimpleIntersect(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;                                                        overload;
function SimpleIntersect(const Point1,Point2,Point3,Point4:TGeoPoint2D):Boolean;                                                  overload;
function SimpleIntersect(const Segment1,Segment2:TGeoSegment2D):Boolean;                                                          overload;

function ThickSegmentIntersect(const x1,y1,x2,y2,x3,y3,x4,y4,Thickness:TGeoFloat):Boolean;                                        overload;
function ThickSegmentIntersect(const Point1,Point2,Point3,Point4:TGeoPoint2D; const Thickness:TGeoFloat):Boolean;                    overload;
function ThickSegmentIntersect(const Segment1,Segment2:TGeoSegment2D;         const Thickness:TGeoFloat):Boolean;                    overload;

function ThickSegmentIntersect(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,        Thickness:TGeoFloat):Boolean;                    overload;
function ThickSegmentIntersect(const Point1,Point2,Point3,Point4:TGeoPoint3D; const Thickness:TGeoFloat):Boolean;                    overload;
function ThickSegmentIntersect(const Segment1,Segment2:TGeoSegment3D;         const Thickness:TGeoFloat):Boolean;                    overload;

procedure IntersectionPoint(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; out Nx,Ny:TGeoFloat);                                           overload;
procedure IntersectionPoint(const P1,P2,P3,P4:TGeoPoint2D; out Nx,Ny:TGeoFloat);                                                     overload;
function  IntersectionPoint(const P1,P2,P3,P4:TGeoPoint2D):TGeoPoint2D;                                                              overload;
function  IntersectionPoint(const Segment1,Segment2:TGeoSegment2D):TGeoPoint2D;                                                      overload;

procedure IntersectionPoint(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,Fuzzy:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);                      overload;
procedure IntersectionPoint(const P1,P2,P3,P4:TGeoPoint3D; const Fuzzy:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);                              overload;
function  IntersectionPoint(const P1,P2,P3,P4:TGeoPoint3D; const Fuzzy:TGeoFloat = Epsilon):TGeoPoint3D;                                overload;
function  IntersectionPoint(const Segment1,Segment2:TGeoSegment3D; const Fuzzy:TGeoFloat = Epsilon):TGeoPoint3D;                        overload;

function  IntersectionPoint(const Line1,Line2:TGeoLine2D):TGeoPoint2D;                                                               overload;
procedure IntersectionPoint(const Circle1,Circle2:TGeoCircle; out Point1,Point2:TGeoPoint2D);                                        overload;
procedure IntersectionPoint(const Segment:TGeoSegment2D; const Triangle:TGeoTriangle2D; out ICnt:Integer; out I1,I2:TGeoPoint2D);       overload;

procedure IntersectionPoint(const Line:TGeoLine3D; const Triangle:TGeoTriangle3D; out IPoint:TGeoPoint3D);                              overload;
procedure IntersectionPoint(const x1,y1,x2,y2,Cx,Cy,Radius:TGeoFloat; out ICnt:Integer; out Ix1,Iy1,Ix2,Iy2:TGeoFloat);              overload;
procedure IntersectionPoint(const Segment:TGeoSegment2D; const Circle:TGeoCircle; out ICnt:Integer; out I1,I2:TGeoPoint2D);             overload;

function  NormalizeAngle  (const Angle : TGeoFloat) : TGeoFloat;
function  VerticalMirror  (const Angle : TGeoFloat) : TGeoFloat;
function  HorizontalMirror(const Angle : TGeoFloat) : TGeoFloat;

function Quadrant(const Angle : TGeoFloat  ):Integer;                                                                             overload;
function Quadrant(const x,y   : TGeoFloat  ):Integer;                                                                             overload;
function Quadrant(const Point : TGeoPoint2D):Integer;                                                                             overload;

function VertexAngle(x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoFloat;                                                                         overload;
function VertexAngle(const Point1,Point2,Point3:TGeoPoint2D):TGeoFloat;                                                              overload;
function VertexAngle(x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):TGeoFloat;                                                                overload;
function VertexAngle(const Point1,Point2,Point3:TGeoPoint3D):TGeoFloat;                                                              overload;

function OrientedVertexAngle(const x1,y1,x2,y2,x3,y3:TGeoFloat;      const Orient : Integer = geoClockwise):TGeoFloat;                  overload;
function OrientedVertexAngle(const Point1,Point2,Point3:TGeoPoint2D; const Orient : Integer = geoClockwise):TGeoFloat;                  overload;

function CartesianAngle(const x,y   : TGeoFloat  ):TGeoFloat;                                                                        overload;
function CartesianAngle(const Point : TGeoPoint2D):TGeoFloat;                                                                        overload;

function RobustCartesianAngle(const x,y   : TGeoFloat;  const Epsilon : TGeoFloat = Epsilon_High):TGeoFloat;                            overload;
function RobustCartesianAngle(const Point : TGeoPoint2D;const Epsilon : TGeoFloat = Epsilon_High):TGeoFloat;                            overload;

function SegmentIntersectAngle(const Point1,Point2,Point3,Point4:TGeoPoint2D):TGeoFloat;                                             overload;
function SegmentIntersectAngle(const Segment1,Segment2:TGeoSegment2D):TGeoFloat;                                                     overload;
function SegmentIntersectAngle(const Point1,Point2,Point3,Point4:TGeoPoint3D):TGeoFloat;                                             overload;
function SegmentIntersectAngle(const Segment1,Segment2:TGeoSegment3D):TGeoFloat;                                                     overload;

function InPortal(const P:TGeoPoint2D):Boolean;                                                                                   overload;
function InPortal(const P:TGeoPoint3D):Boolean;                                                                                   overload;

function HighestPoint(const Polygon  : TGeoPolygon2D):TGeoPoint2D;                                                                   overload;
function HighestPoint(const Point    : array of TGeoPoint2D):TGeoPoint2D;                                                            overload;
function HighestPoint(const Triangle : TGeoTriangle2D):TGeoPoint2D;                                                                  overload;
function HighestPoint(const Triangle : TGeoTriangle3D):TGeoPoint3D;                                                                  overload;
function HighestPoint(const Quadix   : TQuadix2D):TGeoPoint2D;                                                                    overload;
function HighestPoint(const Quadix   : TQuadix3D):TGeoPoint3D;                                                                    overload;

function LowestPoint(const Polygon   : TGeoPolygon2D):TGeoPoint2D;                                                                   overload;
function LowestPoint(const Point     : array of TGeoPoint2D):TGeoPoint2D;                                                            overload;
function LowestPoint(const Triangle  : TGeoTriangle2D):TGeoPoint2D;                                                                  overload;
function LowestPoint(const Triangle  : TGeoTriangle3D):TGeoPoint3D;                                                                  overload;
function LowestPoint(const Quadix    : TQuadix2D):TGeoPoint2D;                                                                    overload;
function LowestPoint(const Quadix    : TQuadix3D):TGeoPoint3D;                                                                    overload;

function MostLeftPoint(const Polygon: TGeoPolygon2D):TGeoPoint2D;                                                                    overload;
function MostLeftPoint(const Point: array of TGeoPoint2D):TGeoPoint2D;                                                               overload;

function MostRightPoint(const Polygon: TGeoPolygon2D):TGeoPoint2D;                                                                   overload;
function MostRightPoint(const Point: array of TGeoPoint2D):TGeoPoint2D;                                                              overload;

function MostUpperRight(const Polygon: TGeoPolygon2D):TGeoPoint2D;                                                                   overload;
function MostUpperRight(const Point: array of TGeoPoint2D):TGeoPoint2D;                                                              overload;

function MostUpperLeft(const Polygon: TGeoPolygon2D):TGeoPoint2D;                                                                    overload;
function MostUpperLeft(const Point: array of TGeoPoint2D):TGeoPoint2D;                                                               overload;

function MostLowerRight(const Polygon: TGeoPolygon2D):TGeoPoint2D;                                                                   overload;
function MostLowerRight(const Point: array of TGeoPoint2D):TGeoPoint2D;                                                              overload;

function MostLowerLeft(const Polygon: TGeoPolygon2D):TGeoPoint2D;                                                                    overload;
function MostLowerLeft(const Point: array of TGeoPoint2D):TGeoPoint2D;                                                               overload;

function GeoMin(const Point1,Point2:TGeoPoint2D):TGeoPoint2D;                                                                           overload;
function GeoMin(const Point1,Point2:TGeoPoint3D):TGeoPoint3D;                                                                           overload;
function GeoMax(const Point1,Point2:TGeoPoint2D):TGeoPoint2D;                                                                           overload;
function GeoMax(const Point1,Point2:TGeoPoint3D):TGeoPoint3D;                                                                           overload;

function Coincident(const Point1,Point2:TGeoPoint2D):Boolean;                                                                     overload;
function Coincident(const Point1,Point2:TGeoPoint3D):Boolean;                                                                     overload;
function Coincident(const Segment1,Segment2:TGeoSegment2D):Boolean;                                                               overload;
function Coincident(const Segment1,Segment2:TGeoSegment3D):Boolean;                                                               overload;
function Coincident(const Triangle1,Triangle2:TGeoTriangle2D):Boolean;                                                            overload;
function Coincident(const Triangle1,Triangle2:TGeoTriangle3D):Boolean;                                                            overload;
function Coincident(const Rect1,Rect2:TGeoRectangle):Boolean;                                                                     overload;
function Coincident(const Quad1,Quad2:TQuadix2D):Boolean;                                                                      overload;
function Coincident(const Quad1,Quad2:TQuadix3D):Boolean;                                                                      overload;
function Coincident(const Circle1,Circle2:TGeoCircle):Boolean;                                                                    overload;
function Coincident(const Sphr1,Sphr2:TGeoSphere):Boolean;                                                                        overload;
function Coincident(const Obj1,Obj2:TGeometricObject):Boolean;                                                                 overload;

function Parallel(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat;       const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;                  overload;
function Parallel(const Point1,Point2,Point3,Point4:TGeoPoint2D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;                  overload;
function Parallel(const Segment1,Segment2:TGeoSegment2D;         const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;                  overload;
function Parallel(const Line1,Line2:TGeoLine2D;                  const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;                  overload;

function Parallel(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;            overload;
function Parallel(const Point1,Point2,Point3,Point4:TGeoPoint3D;       const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;            overload;
function Parallel(const Segment1,Segment2:TGeoSegment3D;               const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;            overload;
function Parallel(const Line1,Line2:TGeoLine3D;                        const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;            overload;

function RobustParallel(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat;       const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;            overload;
function RobustParallel(const Point1,Point2,Point3,Point4:TGeoPoint2D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;            overload;
function RobustParallel(const Segment1,Segment2:TGeoSegment2D;         const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;            overload;
function RobustParallel(const Line1,Line2:TGeoLine2D;                  const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;            overload;

function RobustParallel(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat;const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;
function RobustParallel(const Point1,Point2,Point3,Point4:TGeoPoint3D;      const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;
function RobustParallel(const Segment1,Segment2:TGeoSegment3D;              const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;
function RobustParallel(const Line1,Line2:TGeoLine3D;                       const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;

function Perpendicular(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat;       const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;             overload;
function Perpendicular(const Point1,Point2,Point3,Point4:TGeoPoint2D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;             overload;
function Perpendicular(const Segment1,Segment2:TGeoSegment2D;         const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;             overload;
function Perpendicular(const Line1,Line2:TGeoLine2D;                  const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;             overload;

function Perpendicular(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;
function Perpendicular(const Point1,Point2,Point3,Point4:TGeoPoint3D;       const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;
function Perpendicular(const Segment1,Segment2:TGeoSegment3D;               const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;
function Perpendicular(const Line1,Line2:TGeoLine3D;                        const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;

function RobustPerpendicular(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat;       const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;
function RobustPerpendicular(const Point1,Point2,Point3,Point4:TGeoPoint2D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;
function RobustPerpendicular(const Segment1,Segment2:TGeoSegment2D;         const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;
function RobustPerpendicular(const Line1,Line2:TGeoLine2D;                  const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;       overload;

function RobustPerpendicular(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean; overload;
function RobustPerpendicular(const Point1,Point2,Point3,Point4:TGeoPoint3D;       const Epsilon:TGeoFloat = Epsilon_Medium):Boolean; overload;
function RobustPerpendicular(const Segment1,Segment2:TGeoSegment3D;               const Epsilon:TGeoFloat = Epsilon_Medium):Boolean; overload;
function RobustPerpendicular(const Line1,Line2:TGeoLine3D;                        const Epsilon:TGeoFloat = Epsilon_Medium):Boolean; overload;

function LineToLineIntersect(x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;                                                          overload;
function LineToLineIntersect(Line1,Line2:TGeoLine2D):Boolean;                                                                     overload;

function RectangleToRectangleIntersect(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;                                          overload;
function RectangleToRectangleIntersect(const Rectangle1,Rectangle2:TGeoRectangle):Boolean;                                        overload;

function RectangleWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;                                               overload;
function RectangleWithinRectangle(const Rectangle1,Rectangle2:TGeoRectangle):Boolean;                                             overload;

function CircleWithinRectangle(const x,y,Radius,x1,y1,x2,y2:TGeoFloat):Boolean;                                                   overload;
function CircleWithinRectangle(const Circle:TGeoCircle; const Rectangle:TGeoRectangle):Boolean;                                      overload;

function TriangleWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4,x5,y5:TGeoFloat):Boolean;                                          overload;
function TriangleWithinRectangle(const Triangle:TGeoTriangle2D; const Rectangle:TGeoRectangle):Boolean;                              overload;

function SegmentWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;                                                 overload;
function SegmentWithinRectangle(const Segment:TGeoSegment2D; const Rectangle:TGeoRectangle):Boolean;                                 overload;

function CircleInCircle(const Circle1,Circle2:TGeoCircle):Boolean;
function IsTangent(const Segment:TGeoSegment2D; const Circle:TGeoCircle):Boolean;
function PointOfReflection(const Sx1,Sy1,Sx2,Sy2,P1x,P1y,P2x,P2y:TGeoFloat; out RPx,RPy:TGeoFloat):Boolean;                          overload;
function PointOfReflection(const Segment:TGeoSegment2D; const P1,P2:TGeoPoint2D; out RP:TGeoPoint2D):Boolean;                           overload;

procedure Mirror(const Px,Py,x1,y1,x2,y2:TGeoFloat;     out   Nx,Ny:TGeoFloat);                                                      overload;
function  Mirror(const Point     : TGeoPoint2D;         const Line:TGeoLine2D):TGeoPoint2D;                                             overload;
function  Mirror(const Segment   : TGeoSegment2D;       const Line:TGeoLine2D):TGeoSegment2D;                                           overload;
function  Mirror(const Rectangle : TGeoRectangle;       const Line:TGeoLine2D):TGeoRectangle;                                           overload;
function  Mirror(const Triangle  : TGeoTriangle2D;      const Line:TGeoLine2D):TGeoTriangle2D;                                          overload;
function  Mirror(const Quadix    : TQuadix2D;        const Line:TGeoLine2D):TQuadix2D;                                            overload;
function  Mirror(const Circle    : TGeoCircle;          const Line:TGeoLine2D):TGeoCircle;                                              overload;
function  Mirror(const Obj       : TGeometricObject; const Line:TGeoLine2D):TGeometricObject;                                     overload;

procedure NonSymmetricMirror(const Px,Py,x1,y1,x2,y2:TGeoFloat; const Ratio:TGeoFloat; out   Nx,Ny:TGeoFloat);                          overload;
function  NonSymmetricMirror(const Point:TGeoPoint2D;           const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeoPoint2D;                 overload;
function  NonSymmetricMirror(const Segment:TGeoSegment2D;       const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeoSegment2D;               overload;
function  NonSymmetricMirror(const Rectangle:TGeoRectangle;     const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeoRectangle;               overload;
function  NonSymmetricMirror(const Triangle:TGeoTriangle2D;     const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeoTriangle2D;              overload;
function  NonSymmetricMirror(const Quadix:TQuadix2D;         const Ratio:TGeoFloat; const Line:TGeoLine2D):TQuadix2D;                overload;
function  NonSymmetricMirror(const Circle:TGeoCircle;           const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeoCircle;                  overload;
function  NonSymmetricMirror(const Obj:TGeometricObject;     const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeometricObject;         overload;

function Distance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;                                                                            overload;
function Distance(const Point1,Point2:TGeoPoint2D):TGeoFloat;                                                                        overload;
function Distance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;                                                                      overload;
function Distance(const Point1,Point2:TGeoPoint3D):TGeoFloat;                                                                        overload;
function Distance(const Point:TGeoPoint2D; const Segment:TGeoSegment2D):TGeoFloat;                                                      overload;
function Distance(const Point:TGeoPoint2D; const Rectangle:TGeoRectangle):TGeoFloat;                                                    overload;
function Distance(const Point:TGeoPoint2D; const Triangle:TGeoTriangle2D):TGeoFloat;                                                    overload;
function Distance(const Point:TGeoPoint2D; const Quadix:TQuadix2D):TGeoFloat;                                                        overload;
function Distance(const Line1,Line2:TGeoLine2D):TGeoFloat;                                                                           overload;
function Distance(const Line1,Line2:TGeoLine3D):TGeoFloat;                                                                           overload;
function Distance(const Segment1,Segment2:TGeoSegment2D):TGeoFloat;                                                                  overload;
function Distance(const Segment1,Segment2:TGeoSegment3D):TGeoFloat;                                                                  overload;
function Distance(const Segment:TGeoSegment2D):TGeoFloat;                                                                            overload;
function Distance(const Segment:TGeoSegment3D):TGeoFloat;                                                                            overload;
function Distance(const Segment:TGeoSegment2D; const Triangle:TGeoTriangle2D):TGeoFloat;                                                overload;
function Distance(const Segment:TGeoSegment3D; const Triangle:TGeoTriangle3D):TGeoFloat;                                                overload;
function Distance(const Segment:TGeoSegment2D; const Rectangle:TGeoRectangle):TGeoFloat;                                                overload;
function Distance(const Segment:TGeoSegment2D; const Circle:TGeoCircle):TGeoFloat;                                                      overload;

function Distance(const Triangle1,Triangle2:TGeoTriangle2D):TGeoFloat;                                                               overload;
function Distance(const Triangle:TGeoTriangle2D; const Rectangle:TGeoRectangle):TGeoFloat;                                              overload;
function Distance(const Rectangle1,Rectangle2:TGeoRectangle):TGeoFloat;                                                              overload;
function Distance(const Triangle:TGeoTriangle2D; const Circle:TGeoCircle):TGeoFloat;                                                    overload;
function Distance(const Rectangle:TGeoRectangle; const Circle:TGeoCircle):TGeoFloat;                                                    overload;
function Distance(const Point : TGeoPoint2D; const Circle:TGeoCircle):TGeoFloat;                                                        overload;
function Distance(const Circle1,Circle2:TGeoCircle):TGeoFloat;                                                                       overload;
function Distance(const Sphere1,Sphere2:TGeoSphere):TGeoFloat;                                                                       overload;
function Distance(const Obj1,Obj2:TGeometricObject):TGeoFloat;                                                                    overload;

function LayDistance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;                                                                         overload;
function LayDistance(const Point1,Point2:TGeoPoint2D):TGeoFloat;                                                                     overload;
function LayDistance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;                                                                   overload;
function LayDistance(const Point1,Point2:TGeoPoint3D):TGeoFloat;                                                                     overload;
function LayDistance(const Point:TGeoPoint2D; const Triangle:TGeoTriangle2D):TGeoFloat;                                                 overload;
function LayDistance(const Point:TGeoPoint2D; const Quadix:TQuadix2D):TGeoFloat;                                                     overload;
function LayDistance(const Segment1,Segment2:TGeoSegment2D):TGeoFloat;                                                               overload;
function LayDistance(const Segment1,Segment2:TGeoSegment3D):TGeoFloat;                                                               overload;
function LayDistance(const Line1,Line2:TGeoLine2D):TGeoFloat;                                                                        overload;
function LayDistance(const Line1,Line2:TGeoLine3D):TGeoFloat;                                                                        overload;
function LayDistance(const Segment:TGeoSegment2D):TGeoFloat;                                                                         overload;
function LayDistance(const Segment:TGeoSegment3D):TGeoFloat;                                                                         overload;
function LayDistance(const Segment:TGeoSegment2D; const Triangle:TGeoTriangle2D):TGeoFloat;                                             overload;
function LayDistance(const Segment:TGeoSegment3D; const Triangle:TGeoTriangle3D):TGeoFloat;                                             overload;

function ManhattanDistance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;                                                                   overload;
function ManhattanDistance(const Point1,Point2:TGeoPoint2D):TGeoFloat;                                                               overload;
function ManhattanDistance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;                                                             overload;
function ManhattanDistance(const Point1,Point2:TGeoPoint3D):TGeoFloat;                                                               overload;
function ManhattanDistance(const Segment:TGeoSegment2D):TGeoFloat;                                                                   overload;
function ManhattanDistance(const Segment:TGeoSegment3D):TGeoFloat;                                                                   overload;
function ManhattanDistance(const Circle1,Circle2:TGeoCircle):TGeoFloat;                                                              overload;

function VectorSumDistance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;                                                                   overload;
function VectorSumDistance(const Point1,Point2:TGeoPoint2D):TGeoFloat;                                                               overload;
function VectorSumDistance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;                                                             overload;
function VectorSumDistance(const Point1,Point2:TGeoPoint3D):TGeoFloat;                                                               overload;
function VectorSumDistance(const Segment:TGeoSegment2D):TGeoFloat;                                                                   overload;
function VectorSumDistance(const Segment:TGeoSegment3D):TGeoFloat;                                                                   overload;
function VectorSumDistance(const Circle1,Circle2:TGeoCircle):TGeoFloat;                                                              overload;
function VectorSumDistance(const Obj1,Obj2:TGeometricObject):TGeoFloat;                                                           overload;

function ChebyshevDistance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;                                                                   overload;
function ChebyshevDistance(const Point1,Point2:TGeoPoint2D):TGeoFloat;                                                               overload;
function ChebyshevDistance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;                                                             overload;
function ChebyshevDistance(const Point1,Point2:TGeoPoint3D):TGeoFloat;                                                               overload;
function ChebyshevDistance(const Segment:TGeoSegment2D):TGeoFloat;                                                                   overload;
function ChebyshevDistance(const Segment:TGeoSegment3D):TGeoFloat;                                                                   overload;
function ChebyshevDistance(const Circle1,Circle2:TGeoCircle):TGeoFloat;                                                              overload;

function InverseChebyshevDistance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;                                                            overload;
function InverseChebyshevDistance(const Point1,Point2:TGeoPoint2D):TGeoFloat;                                                        overload;
function InverseChebyshevDistance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;                                                      overload;
function InverseChebyshevDistance(const Point1,Point2:TGeoPoint3D):TGeoFloat;                                                        overload;
function InverseChebyshevDistance(const Segment:TGeoSegment2D):TGeoFloat;                                                            overload;
function InverseChebyshevDistance(const Segment:TGeoSegment3D):TGeoFloat;                                                            overload;
function InverseChebyshevDistance(const Circle1,Circle2:TGeoCircle):TGeoFloat;                                                       overload;

function DistanceSegmentToSegment(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):TGeoFloat;                                                overload;
function DistanceSegmentToSegment(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat):TGeoFloat;                                    overload;

function LayDistanceSegmentToSegment(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):TGeoFloat;                                             overload;
function LayDistanceSegmentToSegment(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat):TGeoFloat;                                 overload;

function DistanceLineToLine(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):TGeoFloat;                                                      overload;
function DistanceLineToLine(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat):TGeoFloat;                                          overload;

function LayDistanceLineToLine(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):TGeoFloat;                                                   overload;
function LayDistanceLineToLine(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat):TGeoFloat;                                       overload;

function TriangleType(const x1,y1,x2,y2,x3,y3:TGeoFloat):eTriangletype;                                                           overload;
function TriangleType(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):eTriangletype;                                                  overload;
function TriangleType(const Point1,Point2,Point3:TGeoPoint2D):eTriangletype;                                                      overload;
function TriangleType(const Point1,Point2,Point3:TGeoPoint3D):eTriangletype;                                                      overload;
function TriangleType(const Triangle:TGeoTriangle2D):eTriangletype;                                                               overload;
function TriangleType(const Triangle:TGeoTriangle3D):eTriangletype;                                                               overload;

function IsEquilateralTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;                                                        overload;
function IsEquilateralTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;                                               overload;
function IsEquilateralTriangle(const Point1,Point2,Point3:TGeoPoint2D):Boolean;                                                   overload;
function IsEquilateralTriangle(const Point1,Point2,Point3:TGeoPoint3D):Boolean;                                                   overload;
function IsEquilateralTriangle(const Triangle:TGeoTriangle2D):Boolean;                                                            overload;
function IsEquilateralTriangle(const Triangle:TGeoTriangle3D):Boolean;                                                            overload;

function IsIsoscelesTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;                                                          overload;
function IsIsoscelesTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;                                                 overload;
function IsIsoscelesTriangle(const Point1,Point2,Point3:TGeoPoint2D):Boolean;                                                     overload;
function IsIsoscelesTriangle(const Point1,Point2,Point3:TGeoPoint3D):Boolean;                                                     overload;
function IsIsoscelesTriangle(const Triangle:TGeoTriangle2D):Boolean;                                                              overload;
function IsIsoscelesTriangle(const Triangle:TGeoTriangle3D):Boolean;                                                              overload;

function IsRightTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;                                                              overload;
function IsRightTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;                                                     overload;
function IsRightTriangle(const Point1,Point2,Point3:TGeoPoint2D):Boolean;                                                         overload;
function IsRightTriangle(const Point1,Point2,Point3:TGeoPoint3D):Boolean;                                                         overload;
function IsRightTriangle(const Triangle:TGeoTriangle2D):Boolean;                                                                  overload;
function IsRightTriangle(const Triangle:TGeoTriangle3D):Boolean;                                                                  overload;

function IsScaleneTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;                                                            overload;
function IsScaleneTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;                                                   overload;
function IsScaleneTriangle(const Point1,Point2,Point3:TGeoPoint2D):Boolean;                                                       overload;
function IsScaleneTriangle(const Point1,Point2,Point3:TGeoPoint3D):Boolean;                                                       overload;
function IsScaleneTriangle(const Triangle:TGeoTriangle2D):Boolean;                                                                overload;
function IsScaleneTriangle(const Triangle:TGeoTriangle3D):Boolean;                                                                overload;

function IsObtuseTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;                                                             overload;
function IsObtuseTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;                                                    overload;
function IsObtuseTriangle(const Point1,Point2,Point3:TGeoPoint2D):Boolean;                                                        overload;
function IsObtuseTriangle(const Point1,Point2,Point3:TGeoPoint3D):Boolean;                                                        overload;
function IsObtuseTriangle(const Triangle:TGeoTriangle2D):Boolean;                                                                 overload;
function IsObtuseTriangle(const Triangle:TGeoTriangle3D):Boolean;                                                                 overload;

function TriangleEdge(const Triangle:TGeoTriangle2D; const Edge:Integer):TGeoSegment2D;                                              overload;
function TriangleEdge(const Triangle:TGeoTriangle3D; const Edge:Integer):TGeoSegment3D;                                              overload;

function RectangleEdge(const Rectangle:TGeoRectangle; const Edge:Integer):TGeoSegment2D;

function PointInTriangle(const Px,Py,x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;                                                        overload;
function PointInTriangle(const x,y:TGeoFloat; const Triangle:TGeoTriangle2D):Boolean;                                                overload;
function PointInTriangle(const Point:TGeoPoint2D; const Triangle:TGeoTriangle2D):Boolean;                                            overload;

function PointInCircle(const Px,Py,Cx,Cy,Radius:TGeoFloat):Boolean;                                                               overload;
function PointInCircle(const Px,Py:TGeoFloat; const Circle:TGeoCircle):Boolean;                                                      overload;
function PointInCircle(const Point:TGeoPoint2D; const Circle:TGeoCircle):Boolean;                                                    overload;

function PointOnCircle(const Px,Py:TGeoFloat; const Circle:TGeoCircle):Boolean;                                                      overload;
function PointOnCircle(const Point:TGeoPoint2D; const Circle:TGeoCircle):Boolean;                                                    overload;

function TriangleInCircle         (const Triangle:TGeoTriangle2D; const Circle:TGeoCircle):Boolean;
function TriangleOutsideCircle    (const Triangle:TGeoTriangle2D; const Circle:TGeoCircle):Boolean;
function TriangleEncompassesCircle(const Triangle:TGeoTriangle2D; const Circle:TGeoCircle):Boolean;
function RectangleInCircle        (const Rectangle:TGeoRectangle; const Circle:TGeoCircle):Boolean;
function RectangleOutsideCircle   (const Rectangle:TGeoRectangle; const Circle:TGeoCircle):Boolean;
function QuadixInCircle           (const Quadix:TQuadix2D;     const Circle:TGeoCircle):Boolean;
function QuadixOutsideCircle      (const Quadix:TQuadix2D;     const Circle:TGeoCircle):Boolean;

function PointInThreePoinTGeoCircle(const Px,Py,x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;                                                overload;
function PointInThreePoinTGeoCircle(const Point,Point1,Point2,Point3:TGeoPoint2D):Boolean;                                           overload;
function PointInThreePoinTGeoCircle(const Point:TGeoPoint2D; const Triangle:TGeoTriangle2D):Boolean;                                    overload;

function PointInRectangle(const Px,Py:TGeoFloat;   const x1,y1,x2,y2:TGeoFloat):Boolean;                                             overload;
function PointInRectangle(const Point:TGeoPoint2D; const x1,y1,x2,y2:TGeoFloat):Boolean;                                             overload;
function PointInRectangle(const Px,Py:TGeoFloat;   const Rectangle:TGeoRectangle):Boolean;                                           overload;
function PointInRectangle(const Point:TGeoPoint2D; const Rectangle:TGeoRectangle):Boolean;                                           overload;

function PointInBox(const Px,Py,Pz:TGeoFloat; const x1,y1,z1,x2,y2,z2:TGeoFloat):Boolean;                                            overload;
function PointInBox(const Point:TGeoPoint3D;  const x1,y1,z1,x2,y2,z2:TGeoFloat):Boolean;                                            overload;
function PointInBox(const Px,Py,Pz:TGeoFloat; const Box:TGeoBox):Boolean;                                                            overload;
function PointInBox(const Point:TGeoPoint3D;  const Box:TGeoBox):Boolean;                                                            overload;

function TriangleInRectangle(const Triangle:TGeoTriangle2D; const Rectangle:TGeoRectangle):Boolean;
function TriangleOutsideRectangle(const Triangle:TGeoTriangle2D; const Rectangle:TGeoRectangle):Boolean;
function QuadixInRectangle(const Quadix:TQuadix2D; const Rectangle:TGeoRectangle):Boolean;
function QuadixOutsideRectangle(const Quadix:TQuadix2D; const Rectangle:TGeoRectangle):Boolean;

function PointInQuadix(const Px,Py,x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;                                                    overload;
function PointInQuadix(const Point,Point1,Point2,Point3,Point4:TGeoPoint2D):Boolean;                                              overload;
function PointInQuadix(const x,y:TGeoFloat; const Quadix:TQuadix2D):Boolean;                                                      overload;
function PointInQuadix(const Point:TGeoPoint2D; const Quadix:TQuadix2D):Boolean;                                                  overload;
function TriangleInQuadix(const Triangle:TGeoTriangle2D; const Quadix:TQuadix2D):Boolean;
function TriangleOutsideQuadix(const Triangle:TGeoTriangle2D; const Quadix:TQuadix2D):Boolean;

function PointInSphere(const x,y,z:TGeoFloat; const Sphere:TGeoSphere):Boolean;                                                      overload;
function PointInSphere(const Point3D:TGeoPoint3D; const Sphere:TGeoSphere):Boolean;                                                  overload;
function PointOnSphere(const Point3D:TGeoPoint3D; const Sphere:TGeoSphere):Boolean;                                                  overload;
function PolyhedronInSphere(const Polygon:TGeoPolyhedron; const Sphere:TGeoSphere):TInclusion;

function PointOnPerimeter(const Px,Py,x1,y1,x2,y2:TGeoFloat):Boolean;                                                             overload;
function PointOnPerimeter(const Px,Py,x1,y1,x2,y2,x3,y3:TGeoFloat; Robust:Boolean = False):Boolean;                               overload;
function PointOnPerimeter(const Px,Py,x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;                                                 overload;

function PointOnPerimeter(const Point:TGeoPoint2D; const Rectangle : TGeoRectangle):Boolean;                                         overload;
function PointOnPerimeter(const Point:TGeoPoint2D; const Triangle  : TGeoTriangle2D):Boolean;                                        overload;
function PointOnPerimeter(const Point:TGeoPoint2D; const Quadix    : TQuadix2D):Boolean;                                          overload;
function PointOnPerimeter(const Point:TGeoPoint2D; const Circle    : TGeoCircle):Boolean;                                            overload;
function PointOnPerimeter(const Point:TGeoPoint3D; const Sphere    : TGeoSphere):Boolean;                                            overload;
function PointOnPerimeter(const Point:TGeoPoint2D; const Polygon   : TGeoPolygon2D):Boolean;                                         overload;
function PointOnPerimeter(const Point:TGeoPoint2D; const Obj       : TGeometricObject):Boolean;                                   overload;
function PointOnPerimeter(const Point:TGeoPoint3D; const Obj       : TGeometricObject):Boolean;                                   overload;

function PointInObject(const Point:TGeoPoint2D; const Segment   : TGeoSegment2D ):Boolean;                                           overload;
function PointInObject(const Point:TGeoPoint2D; const Line      : TGeoLine2D    ):Boolean;                                           overload;
function PointInObject(const Point:TGeoPoint2D; const Rectangle : TGeoRectangle ):Boolean;                                           overload;
function PointInObject(const Point:TGeoPoint2D; const Triangle  : TGeoTriangle2D):Boolean;                                           overload;
function PointInObject(const Point:TGeoPoint2D; const Quadix    : TQuadix2D  ):Boolean;                                           overload;
function PointInObject(const Point:TGeoPoint2D; const Circle    : TGeoCircle    ):Boolean;                                           overload;
function PointInObject(const Point:TGeoPoint2D; const Polygon   : TGeoPolygon2D ):Boolean;                                           overload;
function PointInObject(const Point:TGeoPoint2D; const Obj       : TGeometricObject):Boolean;                                      overload;

function GeometricSpan(const Point: array of TGeoPoint2D):TGeoFloat;                                                                 overload;
function GeometricSpan(const Point: array of TGeoPoint3D):TGeoFloat;                                                                 overload;

procedure CreateEquilateralTriangle(x1,y1,x2,y2:TGeoFloat; out x3,y3:TGeoFloat);                                                     overload;
procedure CreateEquilateralTriangle(const Point1,Point2:TGeoPoint2D; out Point3:TGeoPoint2D);                                        overload;
function  CreateEquilateralTriangle(const x1,y1,x2,y2:TGeoFloat):TGeoTriangle2D;                                                     overload;
function  CreateEquilateralTriangle(const Point1,Point2:TGeoPoint2D):TGeoTriangle2D;                                                 overload;
function  CreateEquilateralTriangle(const Cx,Cy,SideLength : TGeoFloat) : TGeoTriangle2D;                                            overload;
function  CreateEquilateralTriangle(const CenterPoint : TGeoPoint2D; const SideLength : TGeoFloat) : TGeoTriangle2D;                    overload;

procedure TorricelliPoint(const x1,y1,x2,y2,x3,y3:TGeoFloat; out Px,Py:TGeoFloat);                                                   overload;
function  TorricelliPoint(const Point1,Point2,Point3:TGeoPoint2D):TGeoPoint2D;                                                       overload;
function  TorricelliPoint(const Triangle:TGeoTriangle2D):TGeoPoint2D;                                                                overload;

procedure Incenter(const x1,y1,x2,y2,x3,y3:TGeoFloat; out Px,Py:TGeoFloat);                                                          overload;
procedure Incenter(const Triangle:TGeoTriangle2D; out Px,Py:TGeoFloat);                                                              overload;
function  Incenter(const Point1,Point2,Point3:TGeoPoint2D):TGeoPoint2D;                                                              overload;
function  Incenter(const Triangle:TGeoTriangle2D):TGeoPoint2D;                                                                       overload;

procedure Circumcenter(const x1,y1,x2,y2,x3,y3:TGeoFloat; out Px,Py:TGeoFloat);                                                      overload;
function  Circumcenter(const Point1,Point2,Point3:TGeoPoint2D):TGeoPoint2D;                                                          overload;
function  Circumcenter(const Triangle:TGeoTriangle2D):TGeoPoint2D;                                                                   overload;

function Circumcircle(const P1,P2,P3:TGeoPoint2D):TGeoCircle;                                                                        overload;
function Circumcircle(const Triangle:TGeoTriangle2D):TGeoCircle;                                                                     overload;
function InscribedCircle(const x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoCircle;                                                              overload;
function InscribedCircle(const P1,P2,P3:TGeoPoint2D):TGeoCircle;                                                                     overload;
function InscribedCircle(const Triangle:TGeoTriangle2D):TGeoCircle;                                                                  overload;

procedure ClosestPointOnSegmentFromPoint(const x1,y1,x2,y2,Px,Py:TGeoFloat; out Nx,Ny:TGeoFloat);                                    overload;
procedure ClosestPointOnSegmentFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);                        overload;
procedure ClosestPointOnLineFromPoint(const x1,y1,x2,y2,Px,Py:TGeoFloat; out Nx,Ny:TGeoFloat);                                       overload;
procedure ClosestPointOnLineFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);                           overload;

function ClosestPointOnSegmentFromPoint(const x1,y1,x2,y2,Px,Py:TGeoFloat):TGeoPoint2D;                                              overload;
function ClosestPointOnSegmentFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TGeoFloat):TGeoPoint3D;                                     overload;

function ClosestPointOnSegmentFromPoint(const Segment:TGeoSegment2D; const Point:TGeoPoint2D):TGeoPoint2D;                              overload;
function ClosestPointOnSegmentFromPoint(const Segment:TGeoSegment3D; const Point:TGeoPoint3D):TGeoPoint3D;                              overload;

function ClosestPointOnLineFromPoint(const x1,y1,x2,y2,Px,Py:TGeoFloat):TGeoPoint2D;                                                 overload;
function ClosestPointOnLineFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TGeoFloat):TGeoPoint3D;                                        overload;

function ClosestPointOnLineFromPoint(const Line:TGeoLine2D; const Point:TGeoPoint2D):TGeoPoint2D;                                       overload;
function ClosestPointOnLineFromPoint(const Line:TGeoLine3D; const Point:TGeoPoint3D):TGeoPoint3D;                                       overload;

procedure ClosestPointOnTriangleFromPoint(const x1,y1,x2,y2,x3,y3,Px,Py:TGeoFloat; out Nx,Ny:TGeoFloat);                             overload;
function  ClosestPointOnTriangleFromPoint(const Triangle:TGeoTriangle2D;  const Px,Py:TGeoFloat  ):TGeoPoint2D;                         overload;
function  ClosestPointOnTriangleFromPoint(const Triangle:TGeoTriangle2D;  const Point:TGeoPoint2D):TGeoPoint2D;                         overload;
function  ClosestPointOnTriangleFromPoint(const Triangle:TGeoTriangle3D;  const Point:TGeoPoint3D):TGeoPoint3D;                         overload;

procedure ClosestPointOnRectangleFromPoint(const x1,y1,x2,y2,Px,Py:TGeoFloat; out Nx,Ny:TGeoFloat);                                  overload;
function  ClosestPointOnRectangleFromPoint(const Rectangle:TGeoRectangle;  const Px,Py:TGeoFloat  ):TGeoPoint2D;                        overload;
function  ClosestPointOnRectangleFromPoint(const Rectangle:TGeoRectangle;  const Point:TGeoPoint2D):TGeoPoint2D;                        overload;

function ClosestPointOnQuadixFromPoint(const Quadix:TQuadix2D;      const Point:TGeoPoint2D):TGeoPoint2D;                            overload;
function ClosestPointOnQuadixFromPoint(const Quadix:TQuadix3D;      const Point:TGeoPoint3D):TGeoPoint3D;                            overload;
function ClosestPointOnCircleFromPoint(const Circle:TGeoCircle;        const Point:TGeoPoint2D):TGeoPoint2D;
function ClosestPointOnSphereFromPoint(const Sphere:TGeoSphere;        const Point:TGeoPoint3D):TGeoPoint3D;
function ClosestPointOnAABBFromPoint  (const Rectangle: TGeoRectangle; const Point:TGeoPoint2D):TGeoPoint2D;

function ClosestPointOnCircleFromSegment(const Circle:TGeoCircle; Segment:TGeoSegment2D):TGeoPoint2D;
function ClosestPointOnSphereFromSegment(const Sphere:TGeoSphere; Segment:TGeoSegment3D):TGeoPoint3D;

function MinimumDistanceFromPointToSegment(const Px,Py,x1,y1,x2,y2:TGeoFloat):TGeoFloat;                                             overload;
function MinimumDistanceFromPointToSegment(const Point:TGeoPoint2D; const Segment:TGeoSegment2D):TGeoFloat;                             overload;
function MinimumDistanceFromPointToSegment(const Px,Py,Pz,x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;                                    overload;
function MinimumDistanceFromPointToSegment(const Point:TGeoPoint3D; const Segment:TGeoSegment3D):TGeoFloat;                             overload;

function MinimumDistanceFromPointToLine(const Px,Py,x1,y1,x2,y2:TGeoFloat):TGeoFloat;                                                overload;
function MinimumDistanceFromPointToLine(const Point:TGeoPoint2D; const Line:TGeoLine2D):TGeoFloat;                                      overload;
function MinimumDistanceFromPointToLine(const Px,Py,Pz,x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;                                       overload;
function MinimumDistanceFromPointToLine(const Point:TGeoPoint3D; const Line:TGeoLine3D):TGeoFloat;                                      overload;

function MinimumDistanceFromPointToTriangle(const Px,Py,x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoFloat;                                      overload;
function MinimumDistanceFromPointToTriangle(const Point:TGeoPoint2D; const Triangle:TGeoTriangle2D):TGeoFloat;                          overload;

function MinimumDistanceFromPointToRectangle(const Px,Py,x1,y1,x2,y2:TGeoFloat):TGeoFloat;                                           overload;
function MinimumDistanceFromPointToRectangle(const Point:TGeoPoint2D; const Rectangle:TGeoRectangle):TGeoFloat;                         overload;

function MinimumDistanceFromPointToPolygon(const Point:TGeoPoint2D; const Polygon:TGeoPolygon2D): TGeoFloat                             overload;

procedure SegmentMidPoint(const x1,y1,x2,y2:TGeoFloat; out midx,midy:TGeoFloat);                                                     overload;
procedure SegmentMidPoint(const Segment:TGeoSegment2D; out midx,midy:TGeoFloat);                                                     overload;
function  SegmentMidPoint(const P1,P2:TGeoPoint2D):TGeoPoint2D;                                                                      overload;
function  SegmentMidPoint(const Segment:TGeoSegment2D):TGeoPoint2D;                                                                  overload;

procedure SegmentMidPoint(const x1,y1,z1,x2,y2,z2:TGeoFloat; out midx,midy,midz:TGeoFloat);                                          overload;
function  SegmentMidPoint(const P1,P2:TGeoPoint3D):TGeoPoint3D;                                                                      overload;
function  SegmentMidPoint(const Segment:TGeoSegment3D):TGeoPoint3D;                                                                  overload;

procedure Centroid(const x1,y1,x2,y2:TGeoFloat; out x,y:TGeoFloat);                                                                  overload;
function  Centroid(const P1,P2:TGeoPoint2D):TGeoPoint2D;                                                                             overload;
function  Centroid(const Segment:TGeoSegment2D):TGeoPoint2D;                                                                         overload;

procedure Centroid(const x1,y1,x2,y2,x3,y3:TGeoFloat; out x,y:TGeoFloat);                                                            overload;
procedure Centroid(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; out x,y:TGeoFloat);                                                      overload;
procedure Centroid(const Triangle:TGeoTriangle2D; out x,y:TGeoFloat);                                                                overload;
procedure Centroid(const Rectangle:TGeoRectangle; out x,y:TGeoFloat);                                                                overload;
procedure Centroid(const Quadix:TQuadix2D; out x,y:TGeoFloat);                                                                    overload;
function  Centroid(const P1,P2,P3:TGeoPoint2D):TGeoPoint2D;                                                                          overload;
function  Centroid(const P1,P2,P3,P4:TGeoPoint2D):TGeoPoint2D;                                                                       overload;
function  Centroid(const Triangle:TGeoTriangle2D):TGeoPoint2D;                                                                       overload;
function  Centroid(const Quadix:TQuadix2D):TGeoPoint2D;                                                                           overload;
function  Centroid(const Rectangle:TGeoRectangle):TGeoPoint2D;                                                                       overload;

procedure Centroid(const Polygon:TGeoPolygon2D; out x,y:TGeoFloat);                                                                  overload;
function  Centroid(const Polygon :TGeoPolygon2D):TGeoPoint2D;                                                                        overload;
function  Centroid(const Polygon : array of TGeoPoint3D):TGeoPoint3D;                                                                overload;

function PolygonSegmentIntersect(const Segment:TGeoSegment2D; const Polygon: TGeoPolygon2D):Boolean;
function PolygonInPolygon(const Poly1,Poly2: TGeoPolygon2D):Boolean;

function PointInConvexPolygon(const Px,Py:TGeoFloat; const Polygon:TGeoPolygon2D):Boolean;                                           overload;
function PointInConvexPolygon(const Point:TGeoPoint2D; const Polygon:TGeoPolygon2D):Boolean;                                         overload;

function PointInConcavePolygon(const Px,Py:TGeoFloat; const Polygon:TGeoPolygon2D):Boolean;                                          overload;
function PointInConcavePolygon(const Point:TGeoPoint2D; const Polygon:TGeoPolygon2D):Boolean;                                        overload;

function PointOnPolygon(const Px,Py:TGeoFloat;   const Polygon:TGeoPolygon2D):Boolean;                                               overload;
function PointOnPolygon(const Point:TGeoPoint2D; const Polygon:TGeoPolygon2D):Boolean;                                               overload;

function PointInPolygon(const Px,Py:TGeoFloat;             const Polygon:TGeoPolygon2D):Boolean;                                     overload;
function PointInPolygon(const Point:TGeoPoint2D;           const Polygon:TGeoPolygon2D):Boolean;                                     overload;
function PointInPolygon(const Point: array of TGeoPoint2D; const Polygon:TGeoPolygon2D): TGeoBooleanArray;                              overload;

function ConvexQuadix(const Quadix:TQuadix2D):Boolean;
function ComplexPolygon(const Polygon:TGeoPolygon2D):Boolean;
function SimplePolygon(const Polygon:TGeoPolygon2D):Boolean;
function ConvexPolygon(const Polygon:TGeoPolygon2D):Boolean;                                                                      overload;
function ConvexPolygon(const Polygon: array of TGeoPoint2D):Boolean;                                                              overload;
function ConcavePolygon(const Polygon:TGeoPolygon2D):Boolean;
function ConvexPolygonOrientation(const Polygon:TGeoPolygon2D):Integer;
function SimplePolygonOrientation(const Polygon:TGeoPolygon2D):Integer;
function SelfIntersectingPolygon(const Polygon:TGeoPolygon2D):Boolean;

function RectangularHull(const Point: array of TGeoPoint2D):TGeoRectangle;                                                           overload;
function RectangularHull(const Polygon:TGeoPolygon2D):TGeoRectangle;                                                                 overload;
function CircularHull(const Polygon:TGeoPolygon2D):TGeoCircle;
function SphereHull(const Polygon:array of TGeoPoint3D):TGeoSphere;

function CalculateBarycentricBase(const x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoFloat;
function CreateBarycentricUnit(const x1,y1,x2,y2,x3,y3:TGeoFloat):TBarycentricUnit;                                               overload;
function CreateBarycentricUnit(const Triangle : TGeoTriangle2D):TBarycentricUnit;                                                 overload;

procedure ConvertCartesianToBarycentric(const x1,y1,x2,y2,x3,y3,px,py:TGeoFloat; out u,v,w: TGeoFloat);                              overload;
procedure ConvertCartesianToBarycentric(const BU:TBarycentricUnit; Px,Py:TGeoFloat; out u,v,w: TGeoFloat);                           overload;
procedure ConvertCartesianToBarycentric(const BU:TBarycentricUnit; const Point:TGeoPoint2D; out BCrd: TBarycentricTriplet);       overload;
function  ConvertCartesianToBarycentric(const BU:TBarycentricUnit; const Point:TGeoPoint2D): TBarycentricTriplet;                 overload;

procedure ConvertBarycentricToCartesian(const u,v,w,x1,y1,x2,y2,x3,y3:TGeoFloat; out x,y:TGeoFloat);                                 overload;
procedure ConvertBarycentricToCartesian(const u,v,w:TGeoFloat; BU:TBarycentricUnit; out x,y:TGeoFloat);                              overload;
procedure ConvertBarycentricToCartesian(const u,v,w:TGeoFloat; BU:TBarycentricUnit; out Point:TGeoPoint2D);                          overload;
function  ConvertBarycentricToCartesian(const u,v,w:TGeoFloat; BU:TBarycentricUnit):TGeoPoint2D;                                     overload;

function Clip(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; out Cx1,Cy1,Cx2,Cy2:TGeoFloat):Boolean;                                       overload;

function Clip(const Segment:TGeoSegment2D; const Rect     : TGeoRectangle;       out CSegment:TGeoSegment2D):Boolean;                   overload;
function Clip(const Segment:TGeoSegment2D; const Triangle : TGeoTriangle2D;      out CSegment:TGeoSegment2D):Boolean;                   overload;
function Clip(const Segment:TGeoSegment2D; const Quadix   : TQuadix2D;        out CSegment:TGeoSegment2D):Boolean;                   overload;
function Clip(const Segment:TGeoSegment2D; const Circle   : TGeoCircle;          out CSegment:TGeoSegment2D):Boolean;                   overload;
function Clip(const Segment:TGeoSegment2D; const Obj      : TGeometricObject; out CSegment:TGeoSegment2D):Boolean;                   overload;
function Clip(const Rect1,Rect2 : TGeoRectangle;                              out CRect : TGeoRectangle):Boolean;                    overload;

function Area(const Point1,Point2,Point3:TGeoPoint2D):TGeoFloat;                                                                     overload;
function Area(const Point1,Point2,Point3:TGeoPoint3D):TGeoFloat;                                                                     overload;
function Area(const Triangle:TGeoTriangle2D):TGeoFloat;                                                                              overload;
function Area(const Triangle:TGeoTriangle3D):TGeoFloat;                                                                              overload;
function Area(const Quadix:TQuadix2D):TGeoFloat;                                                                                  overload;
function Area(const Quadix:TQuadix3D):TGeoFloat;                                                                                  overload;
function Area(const Rectangle:TGeoRectangle):TGeoFloat;                                                                              overload;
function Area(const Circle:TGeoCircle):TGeoFloat;                                                                                    overload;
function Area(const Polygon:TGeoPolygon2D):TGeoFloat;                                                                                overload;
function Area(const Obj:TGeometricObject):TGeoFloat;                                                                              overload;

function Perimeter(const Triangle  : TGeoTriangle2D):TGeoFloat;                                                                      overload;
function Perimeter(const Triangle  : TGeoTriangle3D):TGeoFloat;                                                                      overload;
function Perimeter(const Quadix    : TQuadix2D):TGeoFloat;                                                                        overload;
function Perimeter(const Quadix    : TQuadix3D):TGeoFloat;                                                                        overload;
function Perimeter(const Rectangle : TGeoRectangle):TGeoFloat;                                                                       overload;
function Perimeter(const Circle    : TGeoCircle):TGeoFloat;                                                                          overload;
function Perimeter(const Polygon   : TGeoPolygon2D):TGeoFloat;                                                                       overload;
function Perimeter(const Obj       : TGeometricObject):TGeoFloat;                                                                 overload;

function SemiPerimeter(const Triangle : TGeoTriangle2D):TGeoFloat;                                                                   overload;
function SemiPerimeter(const Triangle : TGeoTriangle3D):TGeoFloat;                                                                   overload;

procedure Rotate(RotAng:TGeoFloat; const x,y:TGeoFloat; out Nx,Ny:TGeoFloat);                                                           overload;
procedure Rotate(const RotAng:TGeoFloat; const x,y,ox,oy:TGeoFloat; out Nx,Ny:TGeoFloat);                                               overload;

function Rotate(const RotAng:TGeoFloat; const Point:TGeoPoint2D):TGeoPoint2D;                                                           overload;
function Rotate(const RotAng:TGeoFloat; const Point,OPoint:TGeoPoint2D):TGeoPoint2D;                                                    overload;
function Rotate(const RotAng:TGeoFloat; const Segment  : TGeoSegment2D):TGeoSegment2D;                                                  overload;
function Rotate(const RotAng:TGeoFloat; const Segment  : TGeoSegment2D; const OPoint: TGeoPoint2D):TGeoSegment2D;                          overload;
function Rotate(const RotAng:TGeoFloat; const Triangle : TGeoTriangle2D):TGeoTriangle2D;                                                overload;
function Rotate(const RotAng:TGeoFloat; const Triangle : TGeoTriangle2D; const OPoint:TGeoPoint2D):TGeoTriangle2D;                         overload;
function Rotate(const RotAng:TGeoFloat; const Quadix   : TQuadix2D):TQuadix2D;                                                    overload;
function Rotate(const RotAng:TGeoFloat; const Quadix   : TQuadix2D; const OPoint:TGeoPoint2D):TQuadix2D;                             overload;
function Rotate(const RotAng:TGeoFloat; const Polygon  : TGeoPolygon2D):TGeoPolygon2D;                                                  overload;
function Rotate(const RotAng:TGeoFloat; const Polygon  : TGeoPolygon2D; const OPoint:TGeoPoint2D):TGeoPolygon2D;                           overload;
function Rotate(const RotAng:TGeoFloat; const Obj      : TGeometricObject):TGeometricObject;                                      overload;
function Rotate(const RotAng:TGeoFloat; const Obj      : TGeometricObject; const OPoint: TGeoPoint2D):TGeometricObject;              overload;

procedure Rotate(const Rx,Ry,Rz:TGeoFloat; const x,y,z:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);                                              overload;
procedure Rotate(const Rx,Ry,Rz:TGeoFloat; const x,y,z,ox,oy,oz:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);                                     overload;

function Rotate(const Rx,Ry,Rz:TGeoFloat; const Point:TGeoPoint3D):TGeoPoint3D;                                                         overload;
function Rotate(const Rx,Ry,Rz:TGeoFloat; const Point,OPoint:TGeoPoint3D):TGeoPoint3D;                                                  overload;
function Rotate(const Rx,Ry,Rz:TGeoFloat; const Segment  : TGeoSegment3D):TGeoSegment3D;                                                overload;
function Rotate(const Rx,Ry,Rz:TGeoFloat; const Segment  : TGeoSegment3D; const OPoint: TGeoPoint3D):TGeoSegment3D;                        overload;
function Rotate(const Rx,Ry,Rz:TGeoFloat; const Triangle : TGeoTriangle3D):TGeoTriangle3D;                                              overload;
function Rotate(const Rx,Ry,Rz:TGeoFloat; const Triangle : TGeoTriangle3D; const OPoint:TGeoPoint3D):TGeoTriangle3D;                       overload;
function Rotate(const Rx,Ry,Rz:TGeoFloat; const Quadix   : TQuadix3D):TQuadix3D;                                                  overload;
function Rotate(const Rx,Ry,Rz:TGeoFloat; const Quadix   : TQuadix3D; const OPoint:TGeoPoint3D):TQuadix3D;                           overload;
function Rotate(const Rx,Ry,Rz:TGeoFloat; const Polygon  : TGeoPolygon3D):TGeoPolygon3D;                                                overload;
function Rotate(const Rx,Ry,Rz:TGeoFloat; const Polygon  : TGeoPolygon3D; const OPoint:TGeoPoint3D):TGeoPolygon3D;                         overload;
function Rotate(const Rx,Ry,Rz:TGeoFloat; const Obj      : TGeometricObject):TGeometricObject;                                    overload;
function Rotate(const Rx,Ry,Rz:TGeoFloat; const Obj      : TGeometricObject; const OPoint: TGeoPoint3D):TGeometricObject;            overload;

procedure FastRotate(RotAng:Integer; const x,y:TGeoFloat; out Nx,Ny:TGeoFloat);                                                      overload;
procedure FastRotate(RotAng:Integer; x,y,ox,oy:TGeoFloat; out Nx,Ny:TGeoFloat);                                                      overload;

function FastRotate(const RotAng:Integer; const Point:TGeoPoint2D):TGeoPoint2D;                                                      overload;
function FastRotate(const RotAng:Integer; const Point,OPoint:TGeoPoint2D):TGeoPoint2D;                                               overload;
function FastRotate(const RotAng:Integer; const Segment  : TGeoSegment2D):TGeoSegment2D;                                             overload;
function FastRotate(const RotAng:Integer; const Segment  : TGeoSegment2D; const OPoint: TGeoPoint2D):TGeoSegment2D;                     overload;
function FastRotate(const RotAng:Integer; const Triangle : TGeoTriangle2D):TGeoTriangle2D;                                           overload;
function FastRotate(const RotAng:Integer; const Triangle : TGeoTriangle2D; const OPoint:TGeoPoint2D):TGeoTriangle2D;                    overload;
function FastRotate(const RotAng:Integer; const Quadix   : TQuadix2D):TQuadix2D;                                               overload;
function FastRotate(const RotAng:Integer; const Quadix   : TQuadix2D; const OPoint:TGeoPoint2D):TQuadix2D;                        overload;
function FastRotate(const RotAng:Integer; const Polygon  : TGeoPolygon2D):TGeoPolygon2D;                                             overload;
function FastRotate(const RotAng:Integer; const Polygon  : TGeoPolygon2D; const OPoint:TGeoPoint2D):TGeoPolygon2D;                      overload;
function FastRotate(const RotAng:Integer; const Obj      : TGeometricObject):TGeometricObject;                                 overload;
function FastRotate(const RotAng:Integer; const Obj      : TGeometricObject; const OPoint: TGeoPoint2D):TGeometricObject;         overload;

procedure FastRotate(Rx,Ry,Rz:Integer; const x,y,z:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);                                               overload;
procedure FastRotate(const Rx,Ry,Rz:Integer; const x,y,z,ox,oy,oz:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);                                overload;

function FastRotate(const Rx,Ry,Rz:Integer; const Point:TGeoPoint3D):TGeoPoint3D;                                                    overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Point,OPoint:TGeoPoint3D):TGeoPoint3D;                                             overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Segment  : TGeoSegment3D):TGeoSegment3D;                                           overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Segment  : TGeoSegment3D; const OPoint: TGeoPoint3D):TGeoSegment3D;                   overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Triangle : TGeoTriangle3D):TGeoTriangle3D;                                         overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Triangle : TGeoTriangle3D; const OPoint:TGeoPoint3D):TGeoTriangle3D;                  overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Quadix   : TQuadix3D):TQuadix3D;                                             overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Quadix   : TQuadix3D; const OPoint:TGeoPoint3D):TQuadix3D;                      overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Polygon  : TGeoPolygon3D):TGeoPolygon3D;                                           overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Polygon  : TGeoPolygon3D; const OPoint:TGeoPoint3D):TGeoPolygon3D;                    overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Obj      : TGeometricObject):TGeometricObject;                               overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Obj      : TGeometricObject; const OPoint: TGeoPoint3D):TGeometricObject;       overload;

function Translate(const Dx,Dy:TGeoFloat;   const Point     : TGeoPoint2D):TGeoPoint2D;                                                 overload;
function Translate(const Dx,Dy:TGeoFloat;   const Line      : TGeoLine2D):TGeoLine2D;                                                   overload;
function Translate(const Dx,Dy:TGeoFloat;   const Segment   : TGeoSegment2D):TGeoSegment2D;                                             overload;
function Translate(const Dx,Dy:TGeoFloat;   const Triangle  : TGeoTriangle2D):TGeoTriangle2D;                                           overload;
function Translate(const Dx,Dy:TGeoFloat;   const Quadix    : TQuadix2D):TQuadix2D;                                               overload;
function Translate(const Dx,Dy:TGeoFloat;   const Rectangle : TGeoRectangle):TGeoRectangle;                                             overload;
function Translate(const Dx,Dy:TGeoFloat;   const Circle    : TGeoCircle):TGeoCircle;                                                   overload;
function Translate(const Dx,Dy:TGeoFloat;   const Polygon   : TGeoPolygon2D):TGeoPolygon2D;                                             overload;
function Translate(const Point:TGeoPoint2D; const Polygon   : TGeoPolygon2D):TGeoPolygon2D;                                             overload;
function Translate(const Dx,Dy:TGeoFloat;   const Obj       : TGeometricObject):TGeometricObject;                                 overload;

function Translate(const Delta:TGeoFloat;   const Point     : TGeoPoint2D):TGeoPoint2D;                                                 overload;
function Translate(const Delta:TGeoFloat;   const Line      : TGeoLine2D):TGeoLine2D;                                                   overload;
function Translate(const Delta:TGeoFloat;   const Segment   : TGeoSegment2D):TGeoSegment2D;                                             overload;
function Translate(const Delta:TGeoFloat;   const Triangle  : TGeoTriangle2D):TGeoTriangle2D;                                           overload;
function Translate(const Delta:TGeoFloat;   const Quadix    : TQuadix2D):TQuadix2D;                                               overload;
function Translate(const Delta:TGeoFloat;   const Rectangle : TGeoRectangle):TGeoRectangle;                                             overload;
function Translate(const Delta:TGeoFloat;   const Circle    : TGeoCircle):TGeoCircle;                                                   overload;
function Translate(const Delta:TGeoFloat;   const Polygon   : TGeoPolygon2D):TGeoPolygon2D;                                             overload;
function Translate(const Delta:TGeoFloat;   const Obj       : TGeometricObject):TGeometricObject;                                 overload;

function Translate(const Dx,Dy,Dz:TGeoFloat; const Point    : TGeoPoint3D):TGeoPoint3D;                                                 overload;
function Translate(const Dx,Dy,Dz:TGeoFloat; const Line     : TGeoLine3D):TGeoLine3D;                                                   overload;
function Translate(const Dx,Dy,Dz:TGeoFloat; const Segment  : TGeoSegment3D):TGeoSegment3D;                                             overload;
function Translate(const Dx,Dy,Dz:TGeoFloat; const Triangle : TGeoTriangle3D):TGeoTriangle3D;                                           overload;
function Translate(const Dx,Dy,Dz:TGeoFloat; const Quadix   : TQuadix3D):TQuadix3D;                                               overload;
function Translate(const Dx,Dy,Dz:TGeoFloat; const Sphere   : TGeoSphere):TGeoSphere;                                                   overload;
function Translate(const Dx,Dy,Dz:TGeoFloat; const Polygon  : TGeoPolygon3D):TGeoPolygon3D;                                             overload;
function Translate(const Point:TGeoPoint3D;  const Polygon  : TGeoPolygon3D):TGeoPolygon3D;                                             overload;
function Translate(const Dx,Dy,Dz:TGeoFloat; const Obj      : TGeometricObject):TGeometricObject;                                 overload;

function Scale(const Dx,Dy:TGeoFloat; const Point     : TGeoPoint2D):TGeoPoint2D;                                                       overload;
function Scale(const Dx,Dy:TGeoFloat; const Line      : TGeoLine2D):TGeoLine2D;                                                         overload;
function Scale(const Dx,Dy:TGeoFloat; const Segment   : TGeoSegment2D):TGeoSegment2D;                                                   overload;
function Scale(const Dx,Dy:TGeoFloat; const Triangle  : TGeoTriangle2D):TGeoTriangle2D;                                                 overload;
function Scale(const Dx,Dy:TGeoFloat; const Quadix    : TQuadix2D):TQuadix2D;                                                     overload;
function Scale(const Dx,Dy:TGeoFloat; const Rectangle : TGeoRectangle):TGeoRectangle;                                                   overload;
function Scale(const Dr   :TGeoFloat; const Circle    : TGeoCircle):TGeoCircle;                                                         overload;
function Scale(const Dx,Dy:TGeoFloat; const Polygon   : TGeoPolygon2D):TGeoPolygon2D;                                                   overload;
function Scale(const Dx,Dy:TGeoFloat; const Obj       : TGeometricObject):TGeometricObject;                                       overload;

function Scale(const Dx,Dy,Dz:TGeoFloat; const Point    : TGeoPoint3D):TGeoPoint3D;                                                     overload;
function Scale(const Dx,Dy,Dz:TGeoFloat; const Line     : TGeoLine3D):TGeoLine3D;                                                       overload;
function Scale(const Dx,Dy,Dz:TGeoFloat; const Segment  : TGeoSegment3D):TGeoSegment3D;                                                 overload;
function Scale(const Dx,Dy,Dz:TGeoFloat; const Triangle : TGeoTriangle3D):TGeoTriangle3D;                                               overload;
function Scale(const Dx,Dy,Dz:TGeoFloat; const Quadix   : TQuadix3D):TQuadix3D;                                                   overload;
function Scale(const Dr      :TGeoFloat; const Sphere   : TGeoSphere):TGeoSphere;                                                       overload;
function Scale(const Dx,Dy,Dz:TGeoFloat; const Polygon  : TGeoPolygon3D):TGeoPolygon3D;                                                 overload;
function Scale(const Dx,Dy,Dz:TGeoFloat; const Obj      : TGeometricObject):TGeometricObject;                                     overload;

procedure ShearXAxis(const Shear,x,y:TGeoFloat; out Nx,Ny:TGeoFloat);                                                                overload;
function ShearXAxis(const Shear:TGeoFloat; const Point    : TGeoPoint2D):TGeoPoint2D;                                                   overload;
function ShearXAxis(const Shear:TGeoFloat; const Segment  : TGeoSegment2D):TGeoSegment2D;                                               overload;
function ShearXAxis(const Shear:TGeoFloat; const Triangle : TGeoTriangle2D):TGeoTriangle2D;                                             overload;
function ShearXAxis(const Shear:TGeoFloat; const Quadix   : TQuadix2D):TQuadix2D;                                                 overload;
function ShearXAxis(const Shear:TGeoFloat; const Polygon  : TGeoPolygon2D):TGeoPolygon2D;                                               overload;
function ShearXAxis(const Shear:TGeoFloat; const Obj      : TGeometricObject):TGeometricObject;                                   overload;

procedure ShearYAxis(const Shear,x,y:TGeoFloat; out Nx,Ny:TGeoFloat);                                                                overload;
function ShearYAxis(const Shear:TGeoFloat; const Point    : TGeoPoint2D):TGeoPoint2D;                                                   overload;
function ShearYAxis(const Shear:TGeoFloat; const Segment  : TGeoSegment2D):TGeoSegment2D;                                               overload;
function ShearYAxis(const Shear:TGeoFloat; const Triangle : TGeoTriangle2D):TGeoTriangle2D;                                             overload;
function ShearYAxis(const Shear:TGeoFloat; const Quadix   : TQuadix2D):TQuadix2D;                                                 overload;
function ShearYAxis(const Shear:TGeoFloat; const Polygon  : TGeoPolygon2D):TGeoPolygon2D;                                               overload;
function ShearYAxis(const Shear:TGeoFloat; const Obj      : TGeometricObject):TGeometricObject;                                   overload;

function CenterAtLocation(const Point:TGeoPoint2D;       const x,y:TGeoFloat):TGeoPoint2D;                                              overload;
function CenterAtLocation(const Segment:TGeoSegment2D;   const x,y:TGeoFloat):TGeoSegment2D;                                            overload;
function CenterAtLocation(const Triangle:TGeoTriangle2D; const x,y:TGeoFloat):TGeoTriangle2D;                                           overload;
function CenterAtLocation(const Rectangle:TGeoRectangle; const x,y:TGeoFloat):TGeoRectangle;                                            overload;
function CenterAtLocation(const Quadix:TQuadix2D;     const x,y:TGeoFloat):TQuadix2D;                                             overload;
function CenterAtLocation(const Circle:TGeoCircle;       const x,y:TGeoFloat):TGeoCircle;                                               overload;
function CenterAtLocation(const Polygon:TGeoPolygon2D;   const x,y:TGeoFloat):TGeoPolygon2D;                                            overload;

function CenterAtLocation(const Point,                      CPoint:TGeoPoint2D):TGeoPoint2D;                                         overload;
function CenterAtLocation(const Segment:TGeoSegment2D;   const CPoint:TGeoPoint2D):TGeoSegment2D;                                       overload;
function CenterAtLocation(const Triangle:TGeoTriangle2D; const CPoint:TGeoPoint2D):TGeoTriangle2D;                                      overload;
function CenterAtLocation(const Rectangle:TGeoRectangle; const CPoint:TGeoPoint2D):TGeoRectangle;                                       overload;
function CenterAtLocation(const Quadix:TQuadix2D;     const CPoint:TGeoPoint2D):TQuadix2D;                                        overload;
function CenterAtLocation(const Circle:TGeoCircle;       const CPoint:TGeoPoint2D):TGeoCircle;                                          overload;
function CenterAtLocation(const Polygon:TGeoPolygon2D;   const CPoint:TGeoPoint2D):TGeoPolygon2D;                                       overload;

function AABB(const Segment   : TGeoSegment2D   ):TGeoRectangle;                                                                     overload;
function AABB(const Triangle  : TGeoTriangle2D  ):TGeoRectangle;                                                                     overload;
function AABB(const Rectangle : TGeoRectangle   ):TGeoRectangle;                                                                     overload;
function AABB(const Quadix    : TQuadix2D    ):TGeoRectangle;                                                                     overload;
function AABB(const Circle    : TGeoCircle      ):TGeoRectangle;                                                                     overload;
function AABB(const Polygon   : TGeoPolygon2D   ):TGeoRectangle;                                                                     overload;
function AABB(const Curve     : TGeoPoint2DArray):TGeoRectangle;                                                                     overload;

procedure AABB(const Segment   : TGeoSegment2D;    out x1,y1,x2,y2:TGeoFloat);                                                       overload;
procedure AABB(const Triangle  : TGeoTriangle2D;   out x1,y1,x2,y2:TGeoFloat);                                                       overload;
procedure AABB(const Rectangle : TGeoRectangle;    out x1,y1,x2,y2:TGeoFloat);                                                       overload;
procedure AABB(const Quadix    : TQuadix2D;     out x1,y1,x2,y2:TGeoFloat);                                                       overload;
procedure AABB(const Circle    : TGeoCircle;       out x1,y1,x2,y2:TGeoFloat);                                                       overload;
procedure AABB(const Polygon   : TGeoPolygon2D;    out x1,y1,x2,y2:TGeoFloat);                                                       overload;
procedure AABB(const Curve     : TGeoPoint2DArray; out x1,y1,x2,y2:TGeoFloat);                                                       overload;

procedure ProjectPoint(const Srcx,Srcy,Dstx,Dsty,Dist:TGeoFloat; out Nx,Ny:TGeoFloat);                                               overload;
procedure ProjectPoint(const Srcx,Srcy,Srcz,Dstx,Dsty,Dstz,Dist:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);                                  overload;

procedure ProjectPoint(const Px,Py,Angle,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);                                                   overload;
procedure ProjectPoint0  (const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);                                                      overload;
procedure ProjectPoint45 (const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);                                                      overload;
procedure ProjectPoint90 (const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);                                                      overload;
procedure ProjectPoint135(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);                                                      overload;
procedure ProjectPoint180(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);                                                      overload;
procedure ProjectPoint225(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);                                                      overload;
procedure ProjectPoint270(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);                                                      overload;
procedure ProjectPoint315(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);                                                      overload;

function ProjectPoint(const SrcPoint,DstPoint:TGeoPoint2D; const Dist:TGeoFloat):TGeoPoint2D;                                           overload;
function ProjectPoint(const SrcPoint,DstPoint:TGeoPoint3D; const Dist:TGeoFloat):TGeoPoint3D;                                           overload;

function ProjectPoint(const Point:TGeoPoint2D; Angle,Distance:TGeoFloat):TGeoPoint2D;                                                   overload;

function ProjectPoint0  (const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;                                                overload;
function ProjectPoint45 (const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;                                                overload;
function ProjectPoint90 (const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;                                                overload;
function ProjectPoint135(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;                                                overload;
function ProjectPoint180(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;                                                overload;
function ProjectPoint225(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;                                                overload;
function ProjectPoint270(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;                                                overload;
function ProjectPoint315(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;                                                overload;

function ProjectObject(const Point    : TGeoPoint2D;         const Angle,Distance:TGeoFloat):TGeoPoint2D;                               overload;
function ProjectObject(const Segment  : TGeoSegment2D;       const Angle,Distance:TGeoFloat):TGeoSegment2D;                             overload;
function ProjectObject(const Triangle : TGeoTriangle2D;      const Angle,Distance:TGeoFloat):TGeoTriangle2D;                            overload;
function ProjectObject(const Quadix   : TQuadix2D;        const Angle,Distance:TGeoFloat):TQuadix2D;                              overload;
function ProjectObject(const Circle   : TGeoCircle;          const Angle,Distance:TGeoFloat):TGeoCircle;                                overload;
function ProjectObject(const Polygon  : TGeoPolygon2D;       const Angle,Distance:TGeoFloat):TGeoPolygon2D;                             overload;
function ProjectObject(const GeoObj   : TGeometricObject; const Angle,Distance:TGeoFloat):TGeometricObject;                       overload;

procedure CalculateBezierCoefficients(const Bezier : TGeoQuadraticBezier2D; out ax,bx,ay,by:TGeoFloat);                              overload;
procedure CalculateBezierCoefficients(const Bezier : TGeoQuadraticBezier3D; out ax,bx,ay,by,az,bz:TGeoFloat);                        overload;

procedure CalculateBezierCoefficients(const Bezier : TGeoCubicBezier2D; out ax,bx,cx,ay,by,cy:TGeoFloat);                            overload;
procedure CalculateBezierCoefficients(const Bezier : TGeoCubicBezier3D; out ax,bx,cx,ay,by,cy,az,bz,cz:TGeoFloat);                   overload;

function  PointOnBezier(const StartPoint:TGeoPoint2D; const ax,bx,ay,by,T:TGeoFloat):TGeoPoint2D;                                       overload;
function  PointOnBezier(const StartPoint:TGeoPoint3D; const ax,bx,ay,by,az,bz,T:TGeoFloat):TGeoPoint3D;                                 overload;

function  PointOnBezier(const StartPoint:TGeoPoint2D; const ax,bx,cx,ay,by,cy,T:TGeoFloat):TGeoPoint2D;                                 overload;
function  PointOnBezier(const StartPoint:TGeoPoint3D; const ax,bx,cx,ay,by,cy,az,bz,cz,T:TGeoFloat):TGeoPoint3D;                        overload;

function  CreateBezier(const Bezier:TGeoQuadraticBezier2D; const PointCount:Integer):TGeoPoint2DArray;                               overload;
function  CreateBezier(const Bezier:TGeoQuadraticBezier3D; const PointCount:Integer):TGeoPoint3DArray;                               overload;

function  CreateBezier(const Bezier:TGeoCubicBezier2D; const PointCount:Integer):TGeoPoint2DArray;                                   overload;
function  CreateBezier(const Bezier:TGeoCubicBezier3D; const PointCount:Integer):TGeoPoint3DArray;                                   overload;

function  CreateCurvePointBezier(const Bezier:TGeoQuadraticBezier2D; const PointCount:Integer):TGeoCurvePoint2DArray;                overload;
function  CreateCurvePointBezier(const Bezier:TGeoQuadraticBezier3D; const PointCount:Integer):TGeoCurvePoint3DArray;                overload;

function  CreateCurvePointBezier(const Bezier:TGeoCubicBezier2D; const PointCount:Integer):TGeoCurvePoint2DArray;                    overload;
function  CreateCurvePointBezier(const Bezier:TGeoCubicBezier3D; const PointCount:Integer):TGeoCurvePoint3DArray;                    overload;

function CurveLength(const Bezier:TGeoQuadraticBezier2D; const PointCount:Integer):TGeoFloat;                                        overload;
function CurveLength(const Bezier:TGeoQuadraticBezier3D; const PointCount:Integer):TGeoFloat;                                        overload;

function CurveLength(const Bezier:TGeoCubicBezier2D; const PointCount:Integer):TGeoFloat;                                            overload;
function CurveLength(const Bezier:TGeoCubicBezier3D; const PointCount:Integer):TGeoFloat;                                            overload;

procedure ShortenSegment(const Amount:TGeoFloat; var x1,y1,x2,y2:TGeoFloat);                                                         overload;
procedure ShortenSegment(const Amount:TGeoFloat; var x1,y1,z1,x2,y2,z2:TGeoFloat);                                                   overload;
function  ShortenSegment(const Segment:TGeoSegment2D; const Amount:TGeoFloat):TGeoSegment2D;                                            overload;
function  ShortenSegment(const Segment:TGeoSegment3D; const Amount:TGeoFloat):TGeoSegment3D;                                            overload;

procedure LengthenSegment(const Amount:TGeoFloat; out x1,y1,x2,y2:TGeoFloat);                                                        overload;
procedure LengthenSegment(const Amount:TGeoFloat; out x1,y1,z1,x2,y2,z2:TGeoFloat);                                                  overload;
function  LengthenSegment(const Segment:TGeoSegment2D; const Amount:TGeoFloat):TGeoSegment2D;                                           overload;
function  LengthenSegment(const Segment:TGeoSegment3D; const Amount:TGeoFloat):TGeoSegment3D;                                           overload;

function EquatePoint(const x,y:TGeoFloat):TGeoPoint2D;                                                                               overload;
function EquatePoint(const x,y,z:TGeoFloat):TGeoPoint3D;                                                                             overload;
function EquatePointPtr(const x,y:TGeoFloat):TGeoPoint2DPtr;                                                                         overload;
function EquatePointPtr(const x,y,z:TGeoFloat):TGeoPoint3DPtr;                                                                       overload;

procedure EquatePoint(const x,y:TGeoFloat; out Point:TGeoPoint2D);                                                                   overload;
procedure EquatePoint(const x,y,z:TGeoFloat; out Point:TGeoPoint3D);                                                                 overload;
procedure EquatePointPtr(const x,y:TGeoFloat; out Point:TGeoPoint2DPtr);                                                             overload;
procedure EquatePointPtr(const x,y,z:TGeoFloat; out Point:TGeoPoint3DPtr);                                                           overload;

function EquateCurvePoint(x,y,t:TGeoFloat            ):TGeoCurvePoint2D;                                                             overload;
function EquateCurvePoint(x,y,z,t:TGeoFloat          ):TGeoCurvePoint3D;                                                             overload;
function EquateCurvePoint(Point:TGeoPoint2D; t:TGeoFloat):TGeoCurvePoint2D;                                                             overload;
function EquateCurvePoint(Point:TGeoPoint3D; t:TGeoFloat):TGeoCurvePoint3D;                                                             overload;

function EquateSegment(const x1,y1,x2,y2:TGeoFloat):TGeoSegment2D;                                                                   overload;
function EquateSegment(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoSegment3D;                                                             overload;

function EquateSegment(const Point1,Point2:TGeoPoint2D):TGeoSegment2D;                                                               overload;
function EquateSegment(const Point1,Point2:TGeoPoint3D):TGeoSegment3D;                                                               overload;

procedure EquateSegment(const x1,y1,x2,y2:TGeoFloat; out Segment:TGeoSegment2D);                                                     overload;
procedure EquateSegment(const x1,y1,z1,x2,y2,z2:TGeoFloat; out Segment:TGeoSegment3D);                                               overload;

function EquateLine(const x1,y1,x2,y2:TGeoFloat):TGeoLine2D;                                                                         overload;
function EquateLine(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoLine3D;                                                                   overload;

function EquateLine(const Point1,Point2:TGeoPoint2D):TGeoLine2D;                                                                     overload;
function EquateLine(const Point1,Point2:TGeoPoint3D):TGeoLine3D;                                                                     overload;

procedure EquateLine(const x1,y1,x2,y2:TGeoFloat; out Line:TGeoLine2D);                                                              overload;
procedure EquateLine(const x1,y1,z1,x2,y2,z2:TGeoFloat; out Line:TGeoLine3D);                                                        overload;

function EquateQuadix(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):TQuadix2D;                                                         overload;
function EquateQuadix(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat):TQuadix3D;                                             overload;

function EquateQuadix(const Point1,Point2,Point3,Point4:TGeoPoint2D):TQuadix2D;                                                   overload;
function EquateQuadix(const Point1,Point2,Point3,Point4:TGeoPoint3D):TQuadix3D;                                                   overload;

procedure EquateQuadix(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat;             out Quadix:TQuadix2D);                                overload;
procedure EquateQuadix(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; out Quadix:TQuadix3D);                                overload;

function  EquateRectangle(const x1,y1,x2,y2:TGeoFloat):TGeoRectangle;                                                                overload;
function  EquateRectangle(const Point1,Point2:TGeoPoint2D):TGeoRectangle;                                                            overload;
procedure EquateRectangle(const x1,y1,x2,y2:TGeoFloat; out Rect:TGeoRectangle);                                                      overload;
procedure EquateRectangle(const Point1,Point2:TGeoPoint2D; out Rect:TGeoRectangle);                                                  overload;

function EquateTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoTriangle2D;                                                           overload;
function EquateTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):TGeoTriangle3D;                                                  overload;
function EquateTriangle(const Point1,Point2,Point3:TGeoPoint2D):TGeoTriangle2D;                                                      overload;
function EquateTriangle(const Point1,Point2,Point3:TGeoPoint3D):TGeoTriangle3D;                                                      overload;

procedure EquateTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat;          out Triangle:TGeoTriangle2D);                                   overload;
procedure EquateTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat; out Triangle:TGeoTriangle3D);                                   overload;
procedure EquateTriangle(const Point1,Point2,Point3: TGeoPoint2D;    out Triangle:TGeoTriangle2D);                                   overload;
procedure EquateTriangle(const Point1,Point2,Point3: TGeoPoint3D;    out Triangle:TGeoTriangle3D);                                   overload;

function  EquateCircle(const x,y,r:TGeoFloat):TGeoCircle;                                                                            overload;
function  EquateCircle(const Point:TGeoPoint2D; Radius:TGeoFloat):TGeoCircle;                                                           overload;
procedure EquateCircle(const x,y,r:TGeoFloat; out Circle:TGeoCircle);                                                                overload;

function  EquateSphere(const x,y,z,r:TGeoFloat):TGeoSphere;                                                                          overload;
procedure EquateSphere(const x,y,z,r:TGeoFloat; out Sphere:TGeoSphere);                                                              overload;

function EquatePlane(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):TGeoPlane2D;                                                        overload;
function EquatePlane(const Point1,Point2,Point3:TGeoPoint3D):TGeoPlane2D;                                                            overload;

procedure EquatePlane(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat; out Plane:TGeoPlane2D);                                            overload;
procedure EquatePlane(const Point1,Point2,Point3:TGeoPoint3D;     out Plane:TGeoPlane2D);                                            overload;

procedure EquateBezier(const x1,y1,x2,y2,x3,y3:TGeoFloat;          out Bezier:TGeoQuadraticBezier2D);                                overload;
procedure EquateBezier(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat; out Bezier:TGeoQuadraticBezier3D);                                overload;
function  EquateBezier(const Pnt1,Pnt2,Pnt3:TGeoPoint2D):TGeoQuadraticBezier2D;                                                      overload;
function  EquateBezier(const Pnt1,Pnt2,Pnt3:TGeoPoint3D):TGeoQuadraticBezier3D;                                                      overload;

procedure EquateBezier(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat;             out Bezier:TGeoCubicBezier2D);                           overload;
procedure EquateBezier(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; out Bezier:TGeoCubicBezier3D);                           overload;
function  EquateBezier(const Pnt1,Pnt2,Pnt3,Pnt4:TGeoPoint2D):TGeoCubicBezier2D;                                                     overload;
function  EquateBezier(const Pnt1,Pnt2,Pnt3,Pnt4:TGeoPoint3D):TGeoCubicBezier3D;                                                     overload;

function  RectangleToQuadix(x1,y1,x2,y2:TGeoFloat    ):TQuadix2D;                                                                 overload;
function  RectangleToQuadix(Point1,Point2:TGeoPoint2D):TQuadix2D;                                                                 overload;
function  RectangleToQuadix(Rectangle:TGeoRectangle  ):TQuadix2D;                                                                 overload;

function  TriangleToPolygon(x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoPolygon2D;                                                              overload;
function  TriangleToPolygon(Triangle:TGeoTriangle2D):TGeoPolygon2D;                                                                  overload;
function  QuadixToPolygon(x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):TGeoPolygon2D;                                                          overload;
function  QuadixToPolygon(Quadix:TQuadix2D):TGeoPolygon2D;                                                                        overload;
function  CircleToPolygon(const Cx,Cy,Radius:TGeoFloat; const PointCount:Integer):TGeoPolygon2D;                                     overload;
function  CircleToPolygon(const Circle:TGeoCircle;      const PointCount:Integer):TGeoPolygon2D;                                     overload;

procedure SetGeometricObject(const Primitive:TGeoPoint2D;    out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TGeoPoint3D;    out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TGeoLine2D;     out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TGeoLine3D;     out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TGeoSegment2D;  out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TGeoSegment3D;  out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TGeoTriangle2D; out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TGeoTriangle3D; out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TQuadix2D;   out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TQuadix3D;   out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TGeoRectangle;  out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TGeoCircle;     out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TGeoSphere;     out GeoObj:TGeometricObject);                                        overload;

procedure GenerateRandomPoints(const Bx1,By1,Bx2,By2:TGeoFloat; var PointList: TPointList2D);                                 overload;
procedure GenerateRandomPoints(const Rectangle:TGeoRectangle;   var PointList: TPointList2D);                                 overload;
procedure GenerateRandomPoints(const Segment:TGeoSegment2D;     var PointList: TPointList2D);                                 overload;
procedure GenerateRandomPoints(const Segment:TGeoSegment3D;     var PointList: TPointList3D);                                 overload;
procedure GenerateRandomPoints(const Triangle:TGeoTriangle2D;   var PointList: TPointList2D);                                 overload;
procedure GenerateRandomPoints(const Triangle:TGeoTriangle3D;   var PointList: TPointList3D);                                 overload;
procedure GenerateRandomPoints(const Circle:TGeoCircle;         var PointList: TPointList2D);                                 overload;
procedure GenerateRandomPoints(const Quadix:TQuadix2D;       var PointList: TPointList2D);                                 overload;
procedure GenerateRandomPoints(const Quadix:TQuadix3D;       var PointList: TPointList3D);                                 overload;

procedure GenerateRandomPointsOnConvexPentagon(const Pentagon : TGeoPolygon2D; var PointList : TPointList2D);
procedure GenerateRandomPointsOnConvexHexagon (const Hexagon  : TGeoPolygon2D; var PointList : TPointList2D);
procedure GenerateRandomPointsOnConvexHeptagon(const Heptagon : TGeoPolygon2D; var PointList : TPointList2D);
procedure GenerateRandomPointsOnConvexOctagon (const Octagon  : TGeoPolygon2D; var PointList : TPointList2D);

procedure GenerateRandomTriangle(const Bx1,By1,Bx2,By2:TGeoFloat; out Triangle : TGeoTriangle2D);
procedure GenerateRandomQuadix  (const Bx1,By1,Bx2,By2:TGeoFloat; out Quadix   : TQuadix2D);
procedure GenerateRandomCircle  (const Bx1,By1,Bx2,By2:TGeoFloat; out Circle   : TGeoCircle);

function Add(const Vec1,Vec2:TGeoVector2D):TGeoVector2D;                                                                             overload;
function Add(const Vec1,Vec2:TGeoVector3D):TGeoVector3D;                                                                             overload;

function Add(const Vec:TGeoVector2DArray):TGeoVector2D;                                                                              overload;
function Add(const Vec:TGeoVector3DArray):TGeoVector3D;                                                                              overload;
function Add(const Vec1,Vec2:TGeoVector2DArray):TGeoVector2DArray;                                                                   overload;
function Add(const Vec1,Vec2:TGeoVector3DArray):TGeoVector3DArray;                                                                   overload;

function Sub(const Vec1,Vec2:TGeoVector2D):TGeoVector2D;                                                                             overload;
function Sub(const Vec1,Vec2:TGeoVector3D):TGeoVector3D;                                                                             overload;
function Sub(const Vec1,Vec2:TGeoVector2DArray):TGeoVector2DArray;                                                                   overload;
function Sub(const Vec1,Vec2:TGeoVector3DArray):TGeoVector3DArray;                                                                   overload;

function Mul(const Vec1,Vec2:TGeoVector2D):TGeoVector3D;                                                                             overload;
function Mul(const Vec1,Vec2:TGeoVector3D):TGeoVector3D;                                                                             overload;
function Mul(const Vec1,Vec2:TGeoVector3DArray):TGeoVector3DArray;                                                                   overload;

function UniTGeoVector(const Vec:TGeoVector2D):TGeoVector2D;                                                                            overload;
function UniTGeoVector(const Vec:TGeoVector3D):TGeoVector3D;                                                                            overload;

function Magnitude(const Vec:TGeoVector2D):TGeoFloat;                                                                                overload;
function Magnitude(const Vec:TGeoVector3D):TGeoFloat;                                                                                overload;

function DotProduct(const Vec1,Vec2:TGeoVector2D):TGeoFloat;                                                                         overload;
function DotProduct(const Vec1,Vec2:TGeoVector3D):TGeoFloat;                                                                         overload;

function Scale(const Vec:TGeoVector2D; const Factor:TGeoFloat):TGeoVector2D;                                                            overload;
function Scale(const Vec:TGeoVector3D; const Factor:TGeoFloat):TGeoVector3D;                                                            overload;

function Scale(const Vec:TGeoVector2DArray; const Factor:TGeoFloat):TGeoVector2DArray;                                                  overload;
function Scale(const Vec:TGeoVector3DArray; const Factor:TGeoFloat):TGeoVector3DArray;                                                  overload;

function Negate(const Vec:TGeoVector2D):TGeoVector2D;                                                                                overload;
function Negate(const Vec:TGeoVector3D):TGeoVector3D;                                                                                overload;

function Negate(Vec:TGeoVector2DArray):TGeoVector2DArray;                                                                            overload;
function Negate(Vec:TGeoVector3DArray):TGeoVector3DArray;                                                                            overload;

function IsEqual(const Val1,Val2,Epsilon:TGeoFloat):Boolean;                                                                      overload;
function IsEqual(const Point1,Point2:TGeoPoint2D; const Epsilon:TGeoFloat):Boolean;                                                  overload;
function IsEqual(const Point1,Point2:TGeoPoint3D; const Epsilon:TGeoFloat):Boolean;                                                  overload;

function IsEqual(const Val1,Val2:TGeoFloat):Boolean;                                                                              overload;
function IsEqual(const Point1,Point2:TGeoPoint2D):Boolean;                                                                        overload;
function IsEqual(const Point1,Point2:TGeoPoint3D):Boolean;                                                                        overload;

function NotEqual(const Val1,Val2,Epsilon:TGeoFloat):Boolean;                                                                     overload;
function NotEqual(const Point1,Point2:TGeoPoint2D; const Epsilon:TGeoFloat):Boolean;                                                 overload;
function NotEqual(const Point1,Point2:TGeoPoint3D; const Epsilon:TGeoFloat):Boolean;                                                 overload;

function NotEqual(const Val1,Val2:TGeoFloat):Boolean;                                                                             overload;
function NotEqual(const Point1,Point2:TGeoPoint2D):Boolean;                                                                       overload;
function NotEqual(const Point1,Point2:TGeoPoint3D):Boolean;                                                                       overload;

function LessThanOrEqual(const Val1,Val2,Epsilon:TGeoFloat):Boolean;                                                              overload;
function LessThanOrEqual(const Val1,Val2:TGeoFloat):Boolean;                                                                      overload;
function GreaterThanOrEqual(const Val1,Val2,Epsilon:TGeoFloat):Boolean;                                                           overload;
function GreaterThanOrEqual(const Val1,Val2:TGeoFloat):Boolean;                                                                   overload;

function IsEqualZero(const Val,Epsilon:TGeoFloat):Boolean;                                                                        overload;
function IsEqualZero(const Val:TGeoFloat):Boolean;                                                                                overload;

function IsDegenerate(const x1,y1,x2,y2:TGeoFloat):Boolean;                                                                       overload;
function IsDegenerate(const Segment:TGeoSegment2D):Boolean;                                                                       overload;
function IsDegenerate(const Line:TGeoLine2D):Boolean;                                                                             overload;

function IsDegenerate(const x1,y1,z1,x2,y2,z2:TGeoFloat):Boolean;                                                                 overload;
function IsDegenerate(const Segment:TGeoSegment3D):Boolean;                                                                       overload;
function IsDegenerate(const Line:TGeoLine3D):Boolean;                                                                             overload;

function IsDegenerate(const Triangle:TGeoTriangle2D):Boolean;                                                                     overload;
function IsDegenerate(const Triangle:TGeoTriangle3D):Boolean;                                                                     overload;

function IsDegenerate(const Quadix:TQuadix2D):Boolean;                                                                         overload;
function IsDegenerate(const Quadix:TQuadix3D):Boolean;                                                                         overload;

function IsDegenerate(const Rect:TGeoRectangle):Boolean;                                                                          overload;

function IsDegenerate(const Circle:TGeoCircle):Boolean;                                                                           overload;
function IsDegenerate(const Sphere:TGeoSphere):Boolean;                                                                           overload;

function IsDegenerate(const Arc:TGeoCircularArc2D):Boolean;                                                                       overload;

function IsDegenerate(const Obj:TGeometricObject):Boolean;                                                                     overload;

procedure GeoSwap(var val1,val2           :TGeoFloat);                                                                               overload;
procedure GeoSwap(var val1,val2           :Integer);                                                                              overload;
procedure GeoSwap(var Point1,Point2       :TGeoPoint2D);                                                                             overload;
procedure GeoSwap(var Point1,Point2       :TGeoPoint3D);                                                                             overload;
procedure GeoSwap(var Segment1,Segment2   :TGeoSegment2D);                                                                           overload;
procedure GeoSwap(var Segment1,Segment2   :TGeoSegment3D);                                                                           overload;
procedure GeoSwap(var Line1,Line2         :TGeoLine2D);                                                                              overload;
procedure GeoSwap(var Triangle1,Triangle2 :TGeoTriangle2D);                                                                          overload;
procedure GeoSwap(var Triangle1,Triangle2 :TGeoTriangle3D);                                                                          overload;
procedure GeoSwap(var Quadix1,Quadix2     :TQuadix2D);                                                                            overload;
procedure GeoSwap(var Quadix1,Quadix2     :TQuadix3D);                                                                            overload;
procedure GeoSwap(var Circle1,Circle2     :TGeoCircle);                                                                              overload;
procedure GeoSwap(var Sphere1,Sphere2     :TGeoSphere);                                                                              overload;
procedure GeoSwap(var Arc1,Arc2           :TGeoCircularArc2D);                                                                       overload;

function CalculateSystemEpsilon:TGeoFloat;
function ZeroEquivalency:Boolean;
function ExtendedFloatingPointTest:Boolean;
function ExecuteTests:TNumericPrecisionResult;

// calcul de l'altitude d'un point (x, y) dans un triangle
function GeoCalcAltitudeXYInTriangle(const QGeoTriangle3D: TGeoTriangle3D; const QX, QY: double): double; overload;
// calcul de l'altitude d'un point (x, y) dans un triangle
function GeoCalcAltitudeXYInTriangle(const X1, Y1, Z1,
                                           X2, Y2, Z2,
                                           X3, Y3, Z3: double;
                                     const QX, QY: double): double; overload;
// intersection entre un planhorizontal et un segment
function IntersectPlanHorizontalSegment(const QZ: double; const Segmt: TGeoSegment3D; out iX, iY: double): boolean;
// calcul de l'intersection entre un plan vertical passant par (X1, Y1); (X2, Y2) et un triangle 3D
// indispensable pour les profils
// valeur de retour: Le plan passe par le triangle ?
function IntersectPlanVerticalTriangle(const X1, Y1, X2, Y2: double;
                                       const QTriangle3D: TGeoTriangle3D;
                                       out IntersectP1, IntersectP2: TGeoPoint3D): boolean;
// intersection entre un plan de cote Z et un triangle
// valeur de retour: Le plan passe par le triangle ?
function IntersectPlanHorizontalTriangle(const QZ: double;
                                         const QTriangle3D: TGeoTriangle3D;
                                         out IntersectP1, IntersectP2: TGeoPoint3D): boolean;



const PI2       =  6.283185307179586476925286766559000;
const PIDiv180  =  0.017453292519943295769236907684886;
const _180DivPI = 57.295779513082320876798154814105000;
const SQRT_OF_TWO: TGeoFloat = 0.70710678118654752440084436210485;

var

 SystemEpsilon : TGeoFloat;

 (* 2D/3D Portal Definition *)
 MaximumX : TGeoFloat;
 MinimumX : TGeoFloat;
 MaximumY : TGeoFloat;
 MinimumY : TGeoFloat;
 MaximumZ : TGeoFloat;
 MinimumZ : TGeoFloat;


 SinTable : array of TGeoFloat;
 CosTable : array of TGeoFloat;
 TanTable : array of TGeoFloat;

procedure InitialiseTrigonometryTables;
function  RandomValue(ResInt : Integer = RandomResolutionInt; ResFlt : TGeoFloat = RandomResolutionFlt) : TGeoFloat;

implementation

uses
   Math;


function Orientation(const x1,y1,x2,y2,Px,Py:TGeoFloat):Integer;
var
  Orin : TGeoFloat;
begin
  (* Determinant of the 3 points *)
  Orin := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);

  if Orin > Zero then
    Result := geoLeftHandSide          (* Orientaion is to the left-hand side  *)
  else if Orin < Zero then
    Result := geoRightHandSide         (* Orientaion is to the right-hand side *)
  else
    Result := geoCollinearOrientation; (* Orientaion is neutral aka collinear  *)
end;
(* End of Orientation *)


function Orientation(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TGeoFloat):Integer;
var
  Px1  : TGeoFloat;
  Px2  : TGeoFloat;
  Px3  : TGeoFloat;
  Py1  : TGeoFloat;
  Py2  : TGeoFloat;
  Py3  : TGeoFloat;
  Pz1  : TGeoFloat;
  Pz2  : TGeoFloat;
  Pz3  : TGeoFloat;
  Orin : TGeoFloat;
begin
  Px1 := x1 - px;
  Px2 := x2 - px;
  Px3 := x3 - px;

  Py1 := y1 - py;
  Py2 := y2 - py;
  Py3 := y3 - py;

  Pz1 := z1 - pz;
  Pz2 := z2 - pz;
  Pz3 := z3 - pz;

  Orin  := Px1 * (Py2 * Pz3 - Pz2 * Py3) +
           Px2 * (Py3 * Pz1 - Pz3 * Py1) +
           Px3 * (Py1 * Pz2 - Pz1 * Py2);

  if Orin < Zero  then
    Result := geoBelowOrientation      (* Orientaion is below plane                      *)
  else if Orin > Zero then
    Result := geoAboveOrientation      (* Orientaion is above plane                      *)
  else
    Result := geoCoplanarOrientation;  (* Orientaion is coplanar to plane if Result is 0 *)
end;
(* End of Orientation *)


function RobustOrientation(const x1,y1,x2,y2,Px,Py:TGeoFloat):Integer;
var
  Orin : TGeoFloat;
begin
  (* Linear determinant of the 3 points *)
  Orin := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);

  (*
    Calculation Policy:
    if |Orin - Orin`| < Epsilon then Orin` is assumed to be equal to zero.
    Where:
     Orin : is the "real" mathematically precise orientation value, using infinite
            precision arithmetic (hypothetical)
     Orin`: is the calculated imprecise orientation value, using finite precision arithmetic
  *)

  if IsEqual(Orin,Zero) then
    Result := 0                (* Orientaion is neutral aka collinear  *)
  else if Orin < Zero then
    Result := geoRightHandSide    (* Orientaion is to the right-hand side *)
  else
    Result := geoLeftHandSide;    (* Orientaion is to the left-hand side  *)
end;
(* End of Robust Orientation *)


function RobustOrientation(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TGeoFloat):Integer;
var
  Px1  : TGeoFloat;
  Px2  : TGeoFloat;
  Px3  : TGeoFloat;
  Py1  : TGeoFloat;
  Py2  : TGeoFloat;
  Py3  : TGeoFloat;
  Pz1  : TGeoFloat;
  Pz2  : TGeoFloat;
  Pz3  : TGeoFloat;
  Orin : TGeoFloat;
begin
  Px1 := x1 - px;
  Px2 := x2 - px;
  Px3 := x3 - px;

  Py1 := y1 - py;
  Py2 := y2 - py;
  Py3 := y3 - py;

  Pz1 := z1 - pz;
  Pz2 := z2 - pz;
  Pz3 := z3 - pz;

  Orin  := Px1 * (Py2 * Pz3 - Pz2 * Py3) +
           Px2 * (Py3 * Pz1 - Pz3 * Py1) +
           Px3 * (Py1 * Pz2 - Pz1 * Py2);

 if IsEqual(Orin,Zero) then
   Result :=  0             (* Orientaion is coplanar to plane if Result is 0 *)
 else if Orin < Zero then
   Result := -1             (* Orientaion is below plane                      *)
 else
   Result := +1;            (* Orientaion is above plane                      *)
end;
(* End of Robust Orientation *)


function Orientation(const Point1,Point2:TGeoPoint2D; const Px,Py:TGeoFloat):Integer;
begin
  Result := Orientation(Point1.x,Point1.y,Point2.x,Point2.y,Px,Py);
end;
(* End of Orientation *)


function Orientation(const Point1,Point2,Point3:TGeoPoint2D):Integer;
begin
  Result := Orientation(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of Orientation *)


function Orientation(const Line:TGeoLine2D; const Point:TGeoPoint2D):Integer;
begin
  Result := Orientation(Line[1].x,Line[1].y,Line[2].x,Line[2].y,Point.x,Point.y);
end;
(* End of Orientation *)


function Orientation(const Segment:TGeoSegment2D; const Point:TGeoPoint2D):Integer;
begin
  Result := Orientation(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Point.x,Point.y);
end;
(* End of Orientation *)


function Orientation(const Point1,Point2,Point3:TGeoPoint3D; const Px,Py,Pz:TGeoFloat):Integer;
begin
  Result := Orientation(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Px,Py,Pz);
end;
(* End of Orientation *)


function Orientation(const Point1,Point2,Point3,Point4:TGeoPoint3D):Integer;
begin
  Result := Orientation(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z);
end;
(* End of Orientation *)


function Orientation(const Triangle:TGeoTriangle3D; const Point:TGeoPoint3D):Integer;
begin
  Result := Orientation(Triangle[1],Triangle[2],Triangle[3],Point);
end;
(* End of Orientation *)


function Signed(const x1,y1,x2,y2,Px,Py:TGeoFloat):TGeoFloat;
begin
  Result := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);
end;
(* End of Signed *)


function Signed(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TGeoFloat):TGeoFloat;
var
  Px1 : TGeoFloat;
  Px2 : TGeoFloat;
  Px3 : TGeoFloat;
  Py1 : TGeoFloat;
  Py2 : TGeoFloat;
  Py3 : TGeoFloat;
  Pz1 : TGeoFloat;
  Pz2 : TGeoFloat;
  Pz3 : TGeoFloat;
begin
  Px1 := x1 - px;
  Px2 := x2 - px;
  Px3 := x3 - px;

  Py1 := y1 - py;
  Py2 := y2 - py;
  Py3 := y3 - py;

  Pz1 := z1 - pz;
  Pz2 := z2 - pz;
  Pz3 := z3 - pz;

  Result:= Px1 * (Py2 * Pz3 - Pz2 * Py3) +
           Px2 * (Py3 * Pz1 - Pz3 * Py1) +
           Px3 * (Py1 * Pz2 - Pz1 * Py2);
end;
(* End of Signed *)


function Signed(const Point1,Point2:TGeoPoint2D; const Px,Py:TGeoFloat):TGeoFloat;
begin
  Result := Signed(Point1.x,Point1.y,Point2.x,Point2.y,Px,Py);
end;
(* End of Signed *)


function Signed(const Point1,Point2,Point3:TGeoPoint2D):TGeoFloat;
begin
  Result := Signed(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of Signed *)


function Signed(const Line:TGeoLine2D; const Point:TGeoPoint2D):TGeoFloat;
begin
  Result := Signed(Line[1].x,Line[1].y,Line[2].x,Line[2].y,Point.x,Point.y);
end;
(* End of Signed *)


function Signed(const Segment:TGeoSegment2D; const Point:TGeoPoint2D):TGeoFloat;
begin
  Result := Signed(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Point.x,Point.y);
end;
(* End of Signed *)


function Signed(const Point1,Point2,Point3:TGeoPoint3D; const Px,Py,Pz:TGeoFloat):TGeoFloat;
begin
  Result := Signed(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Px,Py,Pz);
end;
(* End of Signed *)


function Signed(const Point1,Point2,Point3,Point4:TGeoPoint3D):TGeoFloat;
begin
  Result := Signed(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z);
end;
(* End of Signed *)


function Signed(const Triangle:TGeoTriangle3D; const Point:TGeoPoint3D):TGeoFloat;
begin
  Result := Signed(Triangle[1],Triangle[2],Triangle[3],Point);
end;
(* End of Signed *)


function Collinear(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;
begin
  Result := IsEqual((x2 - x1) * (y3 - y1) ,(x3 - x1) * (y2 - y1));
end;
(* End of Collinear *)


function Collinear(const x1,y1,x2,y2,x3,y3,Epsilon:TGeoFloat):Boolean;
begin
  Result := IsEqual((x2 - x1) * (y3 - y1),(x3 - x1) * (y2 - y1),Epsilon);
end;
(* End of Collinear *)


function Collinear(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;
var
  Dx1  : TGeoFloat;
  Dx2  : TGeoFloat;
  Dy1  : TGeoFloat;
  Dy2  : TGeoFloat;
  Dz1  : TGeoFloat;
  Dz2  : TGeoFloat;
  Cx   : TGeoFloat;
  Cy   : TGeoFloat;
  Cz   : TGeoFloat;
begin
  (* Find the difference between the 2 points P2 and P3 to P1 *)
  Dx1 := x2 - x1;
  Dy1 := y2 - y1;
  Dz1 := z2 - z1;

  Dx2 := x3 - x1;
  Dy2 := y3 - y1;
  Dz2 := z3 - z1;

  (* Perform a 3d cross product *)
  Cx  := (Dy1 * Dz2) - (Dy2 * Dz1);
  Cy  := (Dx2 * Dz1) - (Dx1 * Dz2);
  Cz  := (Dx1 * Dy2) - (Dx2 * Dy1);

  Result := IsEqual(Cx * Cx + Cy * Cy + Cz * Cz,Zero);
end;
(* End of Collinear *)


function Collinear(const PointA,PointB,PointC:TGeoPoint2D):Boolean;
begin
  Result := Collinear(PointA.x,PointA.y,PointB.x,PointB.y,PointC.x,PointC.y);
end;
(* End of Collinear *)


function Collinear(const PointA,PointB,PointC:TGeoPoint3D):Boolean;
begin
  Result := Collinear(PointA.x,PointA.y,PointA.z,PointB.x,PointB.y,PointB.z,PointC.x,PointC.y,PointC.z);
end;
(* End of Collinear *)


function RobustCollinear(const x1,y1,x2,y2,x3,y3:TGeoFloat; const Epsilon : TGeoFloat = Epsilon_High):Boolean;
var
  LeyDist1 : TGeoFloat;
  LeyDist2 : TGeoFloat;
  LeyDist3 : TGeoFloat;
begin
  LeyDist1 := LayDistance(x1,y1,x2,y2);
  LeyDist2 := LayDistance(x2,y2,x3,y3);
  LeyDist3 := LayDistance(x3,y3,x1,y1);

  if LeyDist1 >= LeyDist2 then
    if LeyDist1 >= LeyDist3 then
      Result := IsEqual(MinimumDistanceFromPointToLine(x3,y3,x1,y1,x2,y2),Zero,Epsilon)
    else
      Result := IsEqual(MinimumDistanceFromPointToLine(x2,y2,x3,y3,x1,y1),Zero,Epsilon)
  else if LeyDist2 >= LeyDist3 then
    Result := IsEqual(MinimumDistanceFromPointToLine(x1,y1,x2,y2,x3,y3),Zero,Epsilon)
  else
    Result := IsEqual(MinimumDistanceFromPointToLine(x2,y2,x3,y3,x1,y1),Zero,Epsilon);
end;
(* End of Robust Collinear *)


function RobustCollinear(const PointA,PointB,PointC:TGeoPoint2D; const Epsilon : TGeoFloat = Epsilon_High):Boolean;
begin
  Result := RobustCollinear(PointA.x,PointA.y,PointB.x,PointB.y,PointC.x,PointC.y,Epsilon);
end;
(* End of Robust Collinear *)

function RobustCollinear(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat; const Epsilon : TGeoFloat = Epsilon_High):Boolean;
var
  LeyDist1 : TGeoFloat;
  LeyDist2 : TGeoFloat;
  LeyDist3 : TGeoFloat;
begin
  LeyDist1 := LayDistance(x1,y1,z1,x2,y2,z2);
  LeyDist2 := LayDistance(x2,y2,z2,x3,y3,z3);
  LeyDist3 := LayDistance(x3,y3,z3,x1,y1,z1);

  if LeyDist1 >= LeyDist2 then
    if LeyDist1 >= LeyDist3 then
      Result := IsEqual(MinimumDistanceFromPointToLine(x3,y3,z3,x1,y1,z1,x2,y2,z2),Zero,Epsilon)
    else
      Result := IsEqual(MinimumDistanceFromPointToLine(x2,y2,z2,x3,y3,z3,x1,y1,z1),Zero,Epsilon)
  else if LeyDist2 >= LeyDist3 then
    Result := IsEqual(MinimumDistanceFromPointToLine(x1,y1,z1,x2,y2,z2,x3,y3,z3),Zero,Epsilon)
  else
    Result := IsEqual(MinimumDistanceFromPointToLine(x2,y2,z2,x3,y3,z3,x1,y1,z1),Zero,Epsilon);
end;
(* End of Robust Collinear *)

function RobustCollinear(const PointA,PointB,PointC:TGeoPoint3D;  const Epsilon : TGeoFloat = Epsilon_High):Boolean;
begin
  Result := RobustCollinear(PointA.x,PointA.y,PointA.z,
                            PointB.x,PointB.y,PointB.z,
                            PointC.x,PointC.y,PointC.z,Epsilon);
end;
(* End of Robust Collinear *)


function Coplanar(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat):Boolean;
begin
  Result := (Orientation(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4) = geoCoplanarOrientation);
end;
(* End of Coplanar *)


function Coplanar(const PointA,PointB,PointC,PointD:TGeoPoint3D):Boolean;
begin
  Result := (Orientation(PointA,PointB,PointC,PointD) = geoCoplanarOrientation);
end;
(* End of Coplanar *)


function IsPointCollinear(const x1,y1,x2,y2,Px,Py:TGeoFloat; const Robust : Boolean = False):Boolean;
begin
  (*
    This method will return true iff the point (px,py) is collinear
    to points (x1,y1) and (x2,y2) and exists on the segment A(x1,y1)->B(x2,y2)
  *)
  if (((x1 <= px) and (px <= x2)) or ((x2 <= px) and (px <= x1))) and
     (((y1 <= py) and (py <= y2)) or ((y2 <= py) and (py <= y1))) then
  begin
    if Robust then
      Result := RobustCollinear(x1,y1,x2,y2,Px,Py)
    else
      Result := Collinear(x1,y1,x2,y2,Px,Py);
  end
  else
    Result := False;
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const PointA,PointB,PointC:TGeoPoint2D; const Robust : Boolean = False):Boolean;
begin
 (*
   This method will return true iff the pointC is collinear
   to points A and B and exists on the segment A->B or B->A
 *)
 Result := IsPointCollinear(PointA.x,PointA.y,PointB.x,PointB.y,PointC.x,PointC.y,Robust);
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const PointA,PointB:TGeoPoint2D; const Px,Py:TGeoFloat; const Robust : Boolean = False):Boolean;
begin
  Result := IsPointCollinear(PointA.x,PointA.y,PointB.x,PointB.y,Px,Py,Robust);
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const Segment:TGeoSegment2D; const PointC:TGeoPoint2D; const Robust : Boolean = False):Boolean;
begin
  Result := IsPointCollinear(Segment[1],Segment[2],PointC,Robust);
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TGeoFloat):Boolean;
begin
 (*
   This method will return true iff the point (px,py,pz) is collinear
   to points (x1,y1,z1) and (x2,y2,z2) and exists on the segment A(x1,y1,z1)->B(x2,y2,z2)
 *)
 if (((x1 <= px) and (px <= x2)) or ((x2 <= px) and (px <= x1))) and
    (((y1 <= py) and (py <= y2)) or ((y2 <= py) and (py <= y1))) and
    (((z1 <= pz) and (pz <= z2)) or ((z2 <= pz) and (pz <= z1))) then
 begin
   Result := Collinear(x1,y1,z1,x2,y2,z2,Px,Py,Pz);
 end
 else
   Result := False;
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const PointA,PointB,PointC:TGeoPoint3D):Boolean;
begin
  (*
    This method will return true iff the pointC is collinear
    to points A and B and exists on the segment A->B or B->A
  *)
  Result := IsPointCollinear(PointA.x,PointA.y,PointA.z,PointB.x,PointB.y,PointB.z,PointC.x,PointC.y,PointC.z);
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const Segment:TGeoSegment3D; const PointC:TGeoPoint3D):Boolean;
begin
  Result := IsPointCollinear(Segment[1],Segment[2],PointC);
end;
(* End of IsPointCollinear *)


function IsPointOnRightSide(const Px,Py,x1,y1,x2,y2:TGeoFloat):Boolean;
begin
  Result := (((x2 - x1) * (py - y1)) < ((px - x1) * (y2 - y1)));
end;
(* End of IsPointOnRightSide *)


function IsPointOnRightSide(const x,y:TGeoFloat; const Segment:TGeoSegment2D):Boolean;
begin
  Result := IsPointOnRightSide(x,y,Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y);
end;
(* End of IsPointOnRightSide *)


function IsPointOnRightSide(const Point:TGeoPoint2D; const Segment:TGeoSegment2D):Boolean;
begin
  Result := IsPointOnRightSide(Point.x,Point.y,Segment);
end;
(* End of IsPointOnRightSide *)


function IsPointOnRightSide(const x,y:TGeoFloat; const Line:TGeoLine2D):Boolean;
begin
  Result := IsPointOnRightSide(x,y,Line[1].x,Line[1].y,Line[2].x,Line[2].y);
end;
(* End of IsPointOnRightSide *)


function IsPointOnRightSide(const Point:TGeoPoint2D; const Line:TGeoLine2D):Boolean;
begin
  Result := IsPointOnRightSide(Point.x,Point.y,Line);
end;
(* End of IsPointOnRightSide *)


function IsPointOnLeftSide(const Px,Py,x1,y1,x2,y2:TGeoFloat):Boolean;
begin
  Result := ((x2 - x1) * (py - y1) > (px - x1) * (y2 - y1));
end;
(* End of IsPointOnLeftSide *)


function IsPointOnLeftSide(const x,y:TGeoFloat; const Segment:TGeoSegment2D):Boolean;
begin
  Result := IsPointOnLeftSide(x,y,Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y);
end;
(* End of IsPointOnLeftSide *)


function IsPointOnLeftSide(const Point:TGeoPoint2D; const Segment:TGeoSegment2D):Boolean;
begin
  Result := IsPointOnLeftSide(Point.x,Point.y,Segment);
end;
(* End of IsPointOnLeftSide *)


function IsPointOnLeftSide(const x,y:TGeoFloat; const Line:TGeoLine2D):Boolean;
begin
  Result := IsPointOnLeftSide(x,y,Line[1].x,Line[1].y,Line[2].x,Line[2].y);
end;
(* End of IsPointOnLeftSide *)


function IsPointOnLeftSide(const Point:TGeoPoint2D; const Line:TGeoLine2D):Boolean;
begin
  Result := IsPointOnLeftSide(Point.x,Point.y,Line);
end;
(* End of IsPointOnLeftSide *)


function RotationCCW(const x1,y1,x2,y2,Px,Py : TGeoFloat):Integer;
begin
  if ((px - x1) * (y2 - y1)) > ((x2 - x1) * (py - y1)) then
    Result := 1
  else
    Result := -1;
end;
(* End of RotationCCW *)


function RotationCW(const x1,y1,x2,y2,Px,Py : TGeoFloat):Integer;
begin
  if ((x2 - x1) * (py - y1)) > ((px - x1) * (y2 - y1)) then
    Result := +1
  else
    Result := -1;
end;
(* End of RotationCW *)


function Intersect(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;
var
  UpperX : TGeoFloat;
  UpperY : TGeoFloat;
  LowerX : TGeoFloat;
  LowerY : TGeoFloat;
  Ax     : TGeoFloat;
  Bx     : TGeoFloat;
  Cx     : TGeoFloat;
  Ay     : TGeoFloat;
  By     : TGeoFloat;
  Cy     : TGeoFloat;
  D      : TGeoFloat;
  F      : TGeoFloat;
  E      : TGeoFloat;
begin
  Result := false;

  Ax := x2 - x1;
  Bx := x3 - x4;

  if Ax < Zero then
  begin
    LowerX := x2;
    UpperX := x1;
  end
  else
  begin
    UpperX := x2;
    LowerX := x1;
  end;

  if Bx > Zero then
  begin
    if (UpperX < x4) or (x3 < LowerX) then
     Exit;
  end
  else if (Upperx < x3) or (x4 < LowerX) then
    Exit;

  Ay := y2 - y1;
  By := y3 - y4;

  if Ay < Zero then
  begin
    LowerY := y2;
    UpperY := y1;
  end
  else
  begin
    UpperY := y2;
    LowerY := y1;
  end;

  if By > Zero then
  begin
    if (UpperY < y4) or (y3 < LowerY) then
      Exit;
  end
  else if (UpperY < y3) or (y4 < LowerY) then
    Exit;

  Cx := x1 - x3;
  Cy := y1 - y3;
  d  := (By * Cx) - (Bx * Cy);
  f  := (Ay * Bx) - (Ax * By);

  if f > Zero then
  begin
    if (d < Zero) or (d > f) then
     Exit;
  end
  else if (d > Zero) or  (d < f) then
    Exit;

  e := (Ax * Cy) - (Ay * Cx);

  if f > Zero then
  begin
    if (e < Zero) or (e > f) then
      Exit;
  end
  else if(e > Zero) or (e < f) then
    Exit;

  Result := true;

  (*

  Simple method, yet not so accurate for certain situations and a little more
  inefficient (roughly 19.5%).
  Result := (
             ((Orientation(x1,y1, x2,y2, x3,y3) * Orientation(x1,y1, x2,y2, x4,y4)) <= 0) and
             ((Orientation(x3,y3, x4,y4, x1,y1) * Orientation(x3,y3, x4,y4, x2,y2)) <= 0)
            );
  *)

end;
(* End of SegmentIntersect *)


function Intersect(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; out ix,iy:TGeoFloat):Boolean;
var
  UpperX    : TGeoFloat;
  UpperY    : TGeoFloat;
  LowerX    : TGeoFloat;
  LowerY    : TGeoFloat;
  Ax        : TGeoFloat;
  Bx        : TGeoFloat;
  Cx        : TGeoFloat;
  Ay        : TGeoFloat;
  By        : TGeoFloat;
  Cy        : TGeoFloat;
  D         : TGeoFloat;
  F         : TGeoFloat;
  E         : TGeoFloat;
  Ratio     : TGeoFloat;
begin
  Result := false;

  Ax := x2 - x1;
  Bx := x3 - x4;

  if Ax < Zero then
  begin
    LowerX := x2;
    UpperX := x1;
  end
  else
  begin
    UpperX := x2;
    LowerX := x1;
  end;

  if Bx > Zero then
  begin
    if (UpperX < x4) or (x3 < LowerX) then
      Exit;
  end
  else if (Upperx < x3) or (x4 < LowerX) then
    Exit;

  Ay := y2 - y1;
  By := y3 - y4;

  if Ay < Zero then
  begin
    LowerY := y2;
    UpperY := y1;
  end
  else
  begin
    UpperY := y2;
    LowerY := y1;
  end;

  if By > Zero then
  begin
  if (UpperY < y4) or (y3 < LowerY) then
    Exit;
  end
  else if (UpperY < y3) or (y4 < LowerY) then
    Exit;

  Cx := x1 - x3;
  Cy := y1 - y3;
  d  := (By * Cx) - (Bx * Cy);
  f  := (Ay * Bx) - (Ax * By);

  if f > Zero then
  begin
    if (d < Zero) or (d > f) then
      Exit;
  end
  else if (d > Zero) or  (d < f) then
    Exit;

  e := (Ax * Cy) - (Ay * Cx);

  if f > Zero then
  begin
    if (e < Zero) or (e > f) then
      Exit;
  end
  else if (e > Zero) or (e < f) then
    Exit;

  Result := true;

  (*

    From IntersectionPoint Routine

    dx1 := x2 - x1; ->  Ax
    dx2 := x4 - x3; -> -Bx
    dx3 := x1 - x3; ->  Cx

    dy1 := y2 - y1; ->  Ay
    dy2 := y1 - y3; ->  Cy
    dy3 := y4 - y3; -> -By

  *)

  Ratio := (Ax * -By) - (Ay * -Bx);

  if NotEqual(Ratio,Zero) then
  begin
    Ratio := ((Cy * -Bx) - (Cx * -By)) / Ratio;
    ix    := x1 + (Ratio * Ax);
    iy    := y1 + (Ratio * Ay);
  end
  else
  begin
    //if Collinear(x1,y1,x2,y2,x3,y3) then
    if IsEqual((Ax * -Cy),(-Cx * Ay)) then
    begin
      ix := x3;
      iy := y3;
    end
    else
    begin
      ix := x4;
      iy := y4;
    end;
  end;
end;
(* End of SegmentIntersect *)


function Intersect(const Point1,Point2,Point3,Point4:TGeoPoint2D):Boolean;
begin
  Result := Intersect(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y);
end;
(* End of Intersect *)


function Intersect(const Segment1,Segment2:TGeoSegment2D):Boolean;
begin
  Result := Intersect(Segment1[1],Segment1[2],Segment2[1],Segment2[2]);
end;
(* End of Intersect *)


function Intersect(const Segment1,Segment2:TGeoSegment2D; out ix, iy : TGeoFloat):Boolean;
begin
  Result := Intersect(Segment1[1].x,Segment1[1].y,Segment1[2].x,Segment1[2].y,Segment2[1].x,Segment2[1].y,Segment2[2].x,Segment2[2].y,ix,iy);
end;
(* End of Intersect *)


function Intersect(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; const fuzzy : TGeoFloat = Zero):Boolean;
begin
  Result := (IsEqual(LayDistanceSegmentToSegment(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4),fuzzy));
end;
(* End of Intersect *)


function Intersect(const P1,P2,P3,P4:TGeoPoint3D; const fuzzy:TGeoFloat = Zero):Boolean;
begin
  Result := Intersect(P1.x,P1.y,P1.z,P2.x,P2.y,P2.z,P3.x,P3.y,P3.z,P4.x,P4.y,P4.z,fuzzy);
end;
(* End of Intersect *)


function Intersect(const Segment1,Segment2:TGeoSegment3D; const fuzzy:TGeoFloat = Zero):Boolean;
begin
  Result := Intersect(Segment1[1],Segment1[2],Segment2[1],Segment2[2],fuzzy);
end;
(* End of Intersect *)


function Intersect(const Segment:TGeoSegment2D; const Rectangle:TGeoRectangle):Boolean;
begin
  Result := RectangleToRectangleIntersect(Rectangle,EquateRectangle(Segment[1],Segment[2]));
end;
(* End of Intersect *)


function Intersect(const Segment:TGeoSegment2D; const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := Intersect(Segment,TriangleEdge(Triangle,1)) or
            Intersect(Segment,TriangleEdge(Triangle,2)) or
            Intersect(Segment,TriangleEdge(Triangle,3)) or
            PointInTriangle(Segment[1],Triangle)        or
            PointInTriangle(Segment[2],Triangle);
end;
(* End of Intersect *)


function Intersect(const Segment:TGeoSegment2D; const Quadix:TQuadix2D):Boolean;
begin
  Result := Intersect(Segment,EquateTriangle(Quadix[1],Quadix[2],Quadix[3])) or
            Intersect(Segment,EquateTriangle(Quadix[1],Quadix[3],Quadix[4]));
end;
(* End of Intersect *)


function Intersect(const Segment:TGeoSegment2D; const Line:TGeoLine2D):Boolean;
begin
  Result := (Orientation(Line,Segment[1]) * Orientation(Line,Segment[2]) <= 0);
end;
(* End of Intersect *)


function Intersect(const Segment:TGeoSegment2D; const Circle:TGeoCircle):Boolean;
var
  Px : TGeoFloat;
  Py : TGeoFloat;
begin
  ClosestPointOnSegmentFromPoint(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Circle.x,Circle.y,Px,Py);
  Result := (LayDistance(Px,Py,Circle.x,Circle.y) <= (Circle.Radius * Circle.Radius));
end;
(* End of Intersect *)


function Intersect(const Segment:TGeoSegment3D; const Sphere:TGeoSphere):Boolean;
var
  A : TGeoFloat;
  B : TGeoFloat;
  C : TGeoFloat;
begin
  A := LayDistance(Segment);
  B := 2 * ((Segment[2].x - Segment[1].x) * (Segment[1].x - Sphere.x) + (Segment[2].y - Segment[1].y) * (Segment[1].y - Sphere.y) + (Segment[2].z - Segment[1].z) * (Segment[1].z  - Sphere.z));
  C := Sqr(Sphere.x) + Sqr(Sphere.y) + Sqr(Sphere.z) + Sqr(Segment[1].x) + Sqr(Segment[1].y) + Sqr(Segment[1].z) - 2 * (Sphere.x * Segment[1].x + Sphere.y * Segment[1].y + Sphere.z * Segment[1].z) - Sqr(Sphere.Radius);
  //Result:=((B * B - 4 * A * C) >= 0)
  Result := GreaterThanOrEqual((B * B - 4 * A * C),Zero)
end;
(* End of Intersect *)


function Intersect(const Line:TGeoLine2D; const Triangle:TGeoTriangle2D):Boolean;
var
  Or1 : Integer;
  Or2 : Integer;
begin
  Result := True;

  Or1 := Orientation (Line[1], Line[2], Triangle[1]);
  if Or1 = 0 then Exit;
  Or2 := Orientation (Line[1], Line[2], Triangle[2]);

  if (Or2 <> Or1) then Exit;
  Or2 := Orientation (Line[1], Line[2], Triangle[3]);

  Result := Or2 <> Or1;
end;
(* End of Intersect *)


function Intersect(const Line:TGeoLine2D; const Quadix:TQuadix2D):Boolean;
var
  Or1 : Integer;
  Or2 : Integer;
begin
  Result := True;

  Or1 := Orientation (Line[1], Line[2], Quadix[1]);
  if Or1 = 0 then Exit;
  Or2 := Orientation (Line[1], Line[2], Quadix[2]);

  if (Or2 <> Or1) then Exit;
  Or2 := Orientation (Line[1], Line[2], Quadix[3]);

  if (Or2 <> Or1) then Exit;
  Or2 := Orientation (Line[1], Line[2], Quadix[4]);

  Result := Or2 <> Or1;
end;
(* End of Intersect *)


function Intersect(const Line:TGeoLine2D; const Circle:TGeoCircle):Boolean;
var
  x1 : TGeoFloat;
  y1 : TGeoFloat;
  x2 : TGeoFloat;
  y2 : TGeoFloat;
begin
  (*
    It is assumed that an intersection of a circle by a line
    is either a full intersection (2 points), partial intersection
    (1 point), or tangential.
    Anything else will Result in a false output.
  *)
  x1 := Line[1].x - Circle.x;
  y1 := Line[1].y - Circle.y;
  x2 := Line[2].x - Circle.x;
  y2 := Line[2].y - Circle.y;
  Result := GreaterThanOrEqual(((Circle.Radius * Circle.Radius) * LayDistance(x1,y1,x2,y2) - Sqr(x1 * y2 - x2 * y1)),Zero);
end;
(* End of Intersect *)


function Intersect(const Line:TGeoLine3D; const Triangle:TGeoTriangle3D; out IPoint: TGeoPoint3D):Boolean;
var
  u   : TGeoVector3D;
  v   : TGeoVector3D;
  n   : TGeoVector3D;
  dir : TGeoVector3D;
  w0  : TGeoVector3D;
  w   : TGeoVector3D;
  a   : TGeoFloat;
  b   : TGeoFloat;
  r   : TGeoFloat;
  uu  : TGeoFloat;
  uv  : TGeoFloat;
  vv  : TGeoFloat;
  wu  : TGeoFloat;
  wv  : TGeoFloat;
  d   : TGeoFloat;
  s   : TGeoFloat;
  t   : TGeoFloat;
begin
  Result := False;

  u.x := Triangle[2].x  - Triangle[1].x;
  u.y := Triangle[2].y  - Triangle[1].y;
  u.z := Triangle[2].z  - Triangle[1].z;

  v.x := Triangle[3].x  - Triangle[1].x;
  v.y := Triangle[3].y  - Triangle[1].y;
  v.z := Triangle[3].z  - Triangle[1].z;

  n   := Mul(u,v);

  if IsEqual(DotProduct(u,n),Zero) then
  begin
   (*
      The Triangle is degenerate, ie: vertices are all
      collinear to each other and/or not unique.
   *)
   Exit;
  end;

  dir.x := Line[2].x - Line[1].x;
  dir.y := Line[2].y - Line[1].y;
  dir.z := Line[2].z - Line[1].z;

  w0.x  := Line[1].x - Triangle[1].x;
  w0.y  := Line[1].y - Triangle[1].y;
  w0.z  := Line[1].z - Triangle[1].z;

  a := dotProduct(n,w0) * -1.0;
  b := dotProduct(n,dir);

  if IsEqual(Abs(b),Zero) then
  begin
    Exit;
    (*
       A further test can be done to determine if the
       ray is coplanar to the Triangle.
       In any case the test for unique point intersection
       has failed.
       if IsEqual(a,Zero) then ray is coplanar to Triangle
    *)
  end;

  r := a / b;

  if IsEqual(r,Zero) then
  begin
    Exit;
  end;

  IPoint.x := Line[1].x + (r * dir.x);
  IPoint.y := Line[1].y + (r * dir.y);
  IPoint.z := Line[1].z + (r * dir.z);

  w.x := IPoint.x - Triangle[1].x;
  w.y := IPoint.y - Triangle[1].y;
  w.z := IPoint.z - Triangle[1].z;

  uu := dotProduct(u,u);
  uv := dotProduct(u,v);
  vv := dotProduct(v,v);
  wu := dotProduct(w,u);
  wv := dotProduct(w,v);

  d  := uv * uv - uu * vv;

  // get and test parametric coords
  s := ((uv * wv) - (vv * wu)) / d;

  if (s < Zero) or (s > 1.0) then
  begin
    (* Intersection is outside of Triangle *)
    Exit;
  end;

  t := ((uv * wu) - (uu * wv)) / d;

  if (t < Zero) or ((s + t) > 1.0) then
  begin
    (* Intersection is outside of Triangle *)
    Exit;
  end;

  Result := True;

end;
(* End of Intersect *)


function Intersect(const Triangle:TGeoTriangle2D; const Circle:TGeoCircle):Boolean;
begin
  Result := PointInCircle(ClosestPointOnTriangleFromPoint(Triangle,Circle.x,Circle.y),Circle);
end;
(* End of Intersect *)


function Intersect(const Triangle:TGeoTriangle2D; const Rectangle : TGeoRectangle):Boolean;
begin
  Result := Intersect(EquateSegment(Triangle[1],Triangle[2]),Rectangle) or
            Intersect(EquateSegment(Triangle[2],Triangle[3]),Rectangle) or
            Intersect(EquateSegment(Triangle[3],Triangle[1]),Rectangle);
end;
(* End of Intersect *)


function Intersect(const Rectangle1,Rectangle2:TGeoRectangle):Boolean;
begin
  Result := RectangleToRectangleIntersect(Rectangle1,Rectangle2);
end;
(* End of Intersect *)


function Intersect(const Triangle1,Triangle2:TGeoTriangle2D):Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 1 to 3 do
  begin
    if IsEqual(MinimumDistanceFromPointToTriangle(Triangle1[i],Triangle2),Zero) or
       IsEqual(MinimumDistanceFromPointToTriangle(Triangle2[i],Triangle1),Zero) then
    begin
      Result := True;
      Exit;
    end;
  end
end;
(* End of Intersect *)


function Intersect(const Rectangle:TGeoRectangle; const Circle:TGeoCircle):Boolean;
begin
  Result := PointInCircle(ClosestPointOnRectangleFromPoint(Rectangle,Circle.x,Circle.y),Circle);
end;
(* End of Intersect *)


function Intersect(const Circle1,Circle2:TGeoCircle):Boolean;
begin
  Result := (LayDistance(Circle1.x,Circle1.y,Circle2.x,Circle2.y) <= ((Circle1.Radius + Circle2.Radius) * (Circle1.Radius + Circle2.Radius)));
end;
(* End of Intersect *)


function Intersect(const Sphere1,Sphere2:TGeoSphere):Boolean;
begin
  Result := (LayDistance(Sphere1.x,Sphere1.y,Sphere1.z,Sphere2.x,Sphere2.y,Sphere2.z) <= ((Sphere1.Radius + Sphere2.Radius) * (Sphere1.Radius + Sphere2.Radius)));
end;
(* End of Intersect *)


function Intersect(const Poly1,Poly2: TGeoPolygon2D):Boolean;
var
  I            : Integer;
  J            : Integer;
  Poly1Trailer : Integer;
  Poly2Trailer : Integer;
begin
  Result := False;
  if (Length(Poly1) < 3) or (Length(Poly2) < 3) then exit;
  Poly1Trailer := Length(Poly1) - 1;
  for i := 0 to Length(Poly1) - 1 do
  begin
    Poly2Trailer := Length(Poly2) - 1;
    for j := 0 to Length(Poly2) - 1 do
    begin
      if Intersect(Poly1[i],Poly1[Poly1Trailer],Poly2[j],Poly2[Poly2Trailer]) then
      begin
        Result := True;
        Exit;
      end;
      Poly2Trailer := j;
    end;
    Poly1Trailer := i;
  end;
end;
(* End of Intersect *)


function Intersect(const Obj1,Obj2:TGeometricObject):Boolean;
begin
  Result := False;
  if Obj1.ObjectType = geoSegment2D then
  begin
    case Obj2.ObjectType of
      geoSegment2D : Result := Intersect(Obj1.Segment2D, Obj2.Segment2D );
      geoRectangle : Result := Intersect(Obj1.Segment2D, Obj2.Rectangle );
      geoTriangle2D: Result := Intersect(Obj1.Segment2D, Obj2.Triangle2D);
      geoQuadix2D  : Result := Intersect(Obj1.Segment2D, Obj2.Quadix2D  );
      geoCircle    : Result := Intersect(Obj1.Segment2D, Obj2.Circle    );
    end;
    Exit;
  end
  else if Obj2.ObjectType = geoSegment2D then
  begin
    case Obj1.ObjectType of
      geoSegment2D : Result := Intersect(Obj2.Segment2D, Obj1.Segment2D );
      geoRectangle : Result := Intersect(Obj2.Segment2D, Obj1.Rectangle );
      geoTriangle2D: Result := Intersect(Obj2.Segment2D, Obj1.Triangle2D);
      geoQuadix2D  : Result := Intersect(Obj2.Segment2D, Obj1.Quadix2D  );
      geoCircle    : Result := Intersect(Obj2.Segment2D, Obj1.Circle    );
    end;
    Exit;
  end
  else if Obj1.ObjectType = geoSegment3D then
  begin
    case Obj2.ObjectType of
      geoSegment3D : Result := Intersect(Obj1.Segment3D, Obj2.Segment3D);
      geoSphere    : Result := Intersect(Obj1.Segment3D, Obj2.Sphere   );
    end;
    Exit;
  end
  else if Obj2.ObjectType = geoSegment3D then
  begin
    case Obj1.ObjectType of
      geoSegment3D : Result := Intersect(Obj2.Segment3D, Obj1.Segment3D);
      geoSphere    : Result := Intersect(Obj2.Segment3D, Obj1.Sphere   );
    end;
    Exit;
  end
  else if Obj1.ObjectType = geoTriangle2D then
  begin
    case Obj2.ObjectType of
      geoRectangle : Result := Intersect(Obj1.Triangle2D, Obj2.Rectangle );
      geoTriangle2D: Result := Intersect(Obj1.Triangle2D, Obj2.Triangle2D);
      geoCircle    : Result := Intersect(Obj1.Triangle2D, Obj2.Circle    );
    end;
    Exit;
  end
  else if Obj2.ObjectType = geoTriangle2D then
  begin
    case Obj1.ObjectType of
      geoRectangle : Result := Intersect(Obj2.Triangle2D, Obj1.Rectangle );
      geoTriangle2D: Result := Intersect(Obj2.Triangle2D, Obj1.Triangle2D);
      geoCircle    : Result := Intersect(Obj2.Triangle2D, Obj1.Circle    );
    end;
    Exit;
  end
  else if Obj1.ObjectType = geoRectangle then
  begin
    case Obj2.ObjectType of
      geoRectangle : Result := Intersect(Obj1.Rectangle,  Obj2.Rectangle);
      geoTriangle2D: Result := Intersect(Obj2.Triangle2D, Obj1.Rectangle);
      geoCircle    : Result := Intersect(Obj1.Rectangle,  Obj2.Circle   );
    end;
    Exit;
  end
  else if Obj2.ObjectType = geoRectangle then
  begin
    case Obj1.ObjectType of
      geoRectangle : Result := Intersect(Obj2.Rectangle,  Obj1.Rectangle);
      geoTriangle2D: Result := Intersect(Obj1.Triangle2D, Obj2.Rectangle);
      geoCircle    : Result := Intersect(Obj2.Rectangle,  Obj1.Circle   );
    end;
    Exit;
  end
  else if (Obj1.ObjectType = geoLine2D) and (Obj2.ObjectType = geoCircle) then
    Result := Intersect(Obj1.Line2D,Obj2.Circle)
  else if (Obj1.ObjectType = geoCircle) and (Obj2.ObjectType = geoLine2D) then
    Result := Intersect(Obj2.Line2D,Obj1.Circle)
  else if (Obj1.ObjectType = geoCircle) and (Obj2.ObjectType = geoCircle) then
    Result := Intersect(Obj1.Circle,Obj2.Circle)
end;
(* End of Intersect *)


function SimpleIntersect(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;
begin
  Result := (
             ((Orientation(x1,y1,x2,y2,x3,y3) * Orientation(x1,y1,x2,y2,x4,y4)) <= 0) and
             ((Orientation(x3,y3,x4,y4,x1,y1) * Orientation(x3,y3,x4,y4,x2,y2)) <= 0)
            );
end;
(* End of SimpleIntersect *)


function SimpleIntersect(const Point1,Point2,Point3,Point4:TGeoPoint2D):Boolean;
begin
  Result := SimpleIntersect(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y);
end;
(* End of SimpleIntersect *)


function SimpleIntersect(const Segment1,Segment2:TGeoSegment2D):Boolean;
begin
  Result := Simpleintersect(Segment1[1],Segment1[2],Segment2[1],Segment2[2]);
end;
(* End of SimpleIntersect *)


function ThickSegmentIntersect(const x1,y1,x2,y2,x3,y3,x4,y4,Thickness:TGeoFloat):Boolean;
begin
  Result := LessThanOrEqual(Thickness * Thickness, LayDistanceSegmentToSegment(x1,y1,x2,y2,x3,y3,x4,y4));
end;
(* End of ThickSegmentIntersect *)


function ThickSegmentIntersect(const Point1,Point2,Point3,Point4:TGeoPoint2D; const Thickness:TGeoFloat):Boolean;
begin
  Result := ThickSegmentIntersect(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y,Thickness);
end;
(* End of ThickSegmentIntersect *)


function ThickSegmentIntersect(const Segment1,Segment2:TGeoSegment2D; const Thickness:TGeoFloat):Boolean;
begin
  Result := ThickSegmentIntersect(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Thickness);
end;
(* End of ThickSegmentIntersect *)


function ThickSegmentIntersect(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,Thickness:TGeoFloat):Boolean;
begin
  Result := LessThanOrEqual(Thickness * Thickness, LayDistanceSegmentToSegment(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4));
end;
(* End of ThickSegmentIntersect *)


function ThickSegmentIntersect(const Point1,Point2,Point3,Point4:TGeoPoint3D; const Thickness:TGeoFloat):Boolean;
begin
  Result := ThickSegmentIntersect(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z,Thickness);
end;
(* End of ThickSegmentIntersect *)


function ThickSegmentIntersect(const Segment1,Segment2:TGeoSegment3D; const Thickness:TGeoFloat):Boolean;
begin
  Result := ThickSegmentIntersect(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Thickness);
end;
(* End of ThickSegmentIntersect *)


procedure IntersectionPoint(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  Ratio : TGeoFloat;
  dx1   : TGeoFloat;
  dx2   : TGeoFloat;
  dx3   : TGeoFloat;
  dy1   : TGeoFloat;
  dy2   : TGeoFloat;
  dy3   : TGeoFloat;
begin
  dx1 := x2 - x1;
  dx2 := x4 - x3;
  dx3 := x1 - x3;

  dy1 := y2 - y1;
  dy2 := y1 - y3;
  dy3 := y4 - y3;

  Ratio := dx1 * dy3 - dy1 * dx2;

  if NotEqual(Ratio,Zero) then
  begin
    Ratio := (dy2 * dx2 - dx3 * dy3) / Ratio;
    Nx    := x1 + Ratio * dx1;
    Ny    := y1 + Ratio * dy1;
  end
  else
  begin
    //if Collinear(x1,y1,x2,y2,x3,y3) then
    if IsEqual((dx1 * -dy2),(-dx3 * dy1)) then
    begin
      Nx := x3;
      Ny := y3;
    end
    else
    begin
      Nx := x4;
      Ny := y4;
    end;
  end;
end;
(* End of IntersectionPoint *)


procedure IntersectionPoint(const P1,P2,P3,P4:TGeoPoint2D; out Nx,Ny:TGeoFloat);
begin
  IntersectionPoint(P1.x,P1.y,P2.x,P2.y,P3.x,P3.y,P4.x,P4.y,Nx,Ny);
end;
(* End of IntersectionPoint *)


function IntersectionPoint(const P1,P2,P3,P4:TGeoPoint2D):TGeoPoint2D;
begin
  IntersectionPoint(P1.x,P1.y,P2.x,P2.y,P3.x,P3.y,P4.x,P4.y,Result.x,Result.y);
end;
(* End of IntersectionPoint *)


function IntersectionPoint(const Segment1,Segment2:TGeoSegment2D):TGeoPoint2D;
begin
  Result := IntersectionPoint(Segment1[1],Segment1[2],Segment2[1],Segment2[2]);
end;
(* End of IntersectionPoint *)

procedure IntersectionPoint(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,Fuzzy:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);
var
  ux, uy, uz,  vx, vy, vz,  wx, wy, wz : TGeoFloat;
  a, b, c, d, e  : TGeoFloat;
  Dt, dx, dy, dz : TGeoFloat;
  sc, sN, sD : TGeoFloat;
  tc, tN, tD : TGeoFloat;
begin
  ux := x2 - x1;
  uy := y2 - y1;
  uz := z2 - z1;

  vx := x4 - x3;
  vy := y4 - y3;
  vz := z4 - z3;

  wx := x1 - x3;
  wy := y1 - y3;
  wz := z1 - z3;

  a  := (ux * ux + uy * uy + uz * uz);
  b  := (ux * vx + uy * vy + uz * vz);
  c  := (vx * vx + vy * vy + vz * vz);
  d  := (ux * wx + uy * wy + uz * wz);
  e  := (vx * wx + vy * wy + vz * wz);
  Dt := a * c - b * b;

  sD := Dt;
  tD := Dt;

  if IsEqual(Dt,Zero) then
  begin
    sN := Zero;
    sD := 1.0;
    tN := e;
    tD := c;
  end
  else
  begin
    sN := (b * e - c * d);
    tN := (a * e - b * d);
    if sN < Zero then
    begin
      sN := Zero;
      tN := e;
      tD := c;
    end
    else if sN > sD then
    begin
      sN := sD;
      tN := e + b;
      tD := c;
    end;
  end;

  if tN < Zero then
  begin
    tN := Zero;
    if -d < Zero then
      sN := Zero
    else if -d > a then
      sN := sD
    else
    begin
      sN := -d;
      sD := a;
    end;
  end
  else if tN > tD  then
  begin
    tN := tD;
    if (-d + b) < Zero then
      sN := Zero
    else if (-d + b) > a then
      sN := sD
    else
    begin
      sN := (-d + b);
      sD := a;
    end;
  end;

  if IsEqual(sN,Zero) then
    sc := Zero
  else
    sc := sN / sD;

  if IsEqual(tN,Zero) then
    tc := Zero
  else
    tc := tN / tD;

  dx := wx + (sc * ux) - (tc * vx);
  dy := wy + (sc * uy) - (tc * vy);
  dz := wz + (sc * uz) - (tc * vz);

  if (dx * dx + dy * dy + dz * dz) <= Sqr(fuzzy) then
  begin
    Nx := ((x1 + (sc * ux)) + (x3 + (tc * vx))) * 0.5;
    Ny := ((y1 + (sc * uy)) + (y3 + (tc * vy))) * 0.5;
    Nz := ((z1 + (sc * uz)) + (z3 + (tc * vz))) * 0.5;
  end
  else
  begin
    Nx := Infinity;
    Ny := Infinity;
    Nz := Infinity;
  end
end;
(* End of IntersectionPoint *)

procedure IntersectionPoint(const P1,P2,P3,P4:TGeoPoint3D; const Fuzzy:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);
begin
  IntersectionPoint(P1.x,P1.y,P1.z,P2.x,P2.y,P2.z,P3.x,P3.y,P3.z,P4.x,P4.y,P4.z,Fuzzy,Nx,Ny,Nz);
end;
(* End of IntersectionPoint *)

function IntersectionPoint(const P1,P2,P3,P4:TGeoPoint3D; const Fuzzy:TGeoFloat = Epsilon):TGeoPoint3D;
begin
  IntersectionPoint(P1.x,P1.y,P1.z,P2.x,P2.y,P2.z,P3.x,P3.y,P3.z,P4.x,P4.y,P4.z,Fuzzy,Result.x,Result.y,Result.z);
end;
(* End of IntersectionPoint *)

function IntersectionPoint(const Segment1,Segment2:TGeoSegment3D; const Fuzzy:TGeoFloat = Epsilon):TGeoPoint3D;
begin
  Result := IntersectionPoint(Segment1[1],Segment1[2],Segment2[1],Segment2[2]);
end;
(* End of IntersectionPoint *)

function IntersectionPoint(const Line1,Line2:TGeoLine2D):TGeoPoint2D;
var
  dx1   : TGeoFloat;
  dx2   : TGeoFloat;
  dx3   : TGeoFloat;
  dy1   : TGeoFloat;
  dy2   : TGeoFloat;
  dy3   : TGeoFloat;
  det   : TGeoFloat;
  ratio : TGeoFloat;
begin
  dx1 := Line1[1].x - Line1[2].x;
  dx2 := Line2[1].x - Line2[2].x;
  dx3 := Line2[2].x - Line1[2].x;
  dy1 := Line1[1].y - Line1[2].y;
  dy2 := Line2[1].y - Line2[2].y;
  dy3 := Line2[2].y - Line1[2].y;

  det := (dx2 * dy1) - (dy2 * dx1);

  if IsEqual(det,Zero) then
  begin
    if IsEqual((dx2 * dy3),(dy2 * dx3)) then
    begin
      Result.x := Line2[1].x;
      Result.y := Line2[1].y;
      Exit;
    end
  else
    Exit;
  end;

  ratio := ((dx1 * dy3) - (dy1 * dx3)) / det;

  Result.x := (ratio * dx2) + Line2[2].x;
  Result.y := (ratio * dy2) + Line2[2].y;

end;
(* End of IntersectionPoint *)


procedure IntersectionPoint(const Circle1,Circle2:TGeoCircle; out Point1,Point2:TGeoPoint2D);
var
  Dist   : TGeoFloat;
  A      : TGeoFloat;
  H      : TGeoFloat;
  RatioA : TGeoFloat;
  RatioH : TGeoFloat;
  Dx     : TGeoFloat;
  Dy     : TGeoFloat;
  Phi    : TGeoPoint2D;
  r1Sqr  : TGeoFloat;
  r2Sqr  : TGeoFloat;
  dstSqr : TGeoFloat;
begin
  Dist   := Distance(Circle1.x,Circle1.y,Circle2.x,Circle2.y);

  dstSqr := Dist * Dist;
  r1Sqr  := Circle1.Radius * Circle1.Radius;
  r2Sqr  := Circle2.Radius * Circle2.Radius;

  A      := (dstSqr - r2sqr + r1sqr) / (2 * Dist);
  H      := Sqrt(r1sqr - (A*A));

  RatioA := A / Dist;
  RatioH := H / Dist;

  Dx     := Circle2.x - Circle1.x;
  Dy     := Circle2.y - Circle1.y;

  Phi.x  := Circle1.x + (RatioA * Dx);
  Phi.y  := Circle1.y + (RatioA * Dy);

  Dx     := Dx * RatioH;
  Dy     := Dy * RatioH;

  Point1.x := Phi.x + Dy;
  Point1.y := Phi.y - Dx;

  Point2.x := Phi.x - Dy;
  Point2.y := Phi.y + Dx;
end;
(* End of IntersectionPoint *)


procedure IntersectionPoint(const Segment:TGeoSegment2D; const Triangle:TGeoTriangle2D; out ICnt:Integer; out I1,I2:TGeoPoint2D);
var
  Ix : TGeoFloat;
  Iy : TGeoFloat;
begin
  ICnt := 0;
  if Intersect(Segment,TriangleEdge(Triangle,1),Ix,Iy) then
  begin
    I1.x := Ix;
    I1.y := Iy;
    Inc(ICnt);
  end;
  if Intersect(Segment,TriangleEdge(Triangle,2),Ix,Iy) then
  begin
    if Icnt = 1 then
    begin
      I2.x := Ix;
      I2.y := Iy;
      Inc(ICnt);
      Exit;
    end;
    I1.x := Ix;
    I1.y := Iy;
    Inc(ICnt);
  end;
  if Intersect(Segment,TriangleEdge(Triangle,3),Ix,Iy) then
  begin
    if Icnt = 1 then
    begin
      I2.x := Ix;
      I2.y := Iy;
      Inc(ICnt);
      Exit;
    end;
    I1.x := Ix;
    I1.y := Iy;
    Inc(ICnt);
  end;
end;
(* End of IntersectionPoint *)


procedure IntersectionPoint(const Line:TGeoLine3D; const Triangle:TGeoTriangle3D; out IPoint: TGeoPoint3D);
var
  u   : TGeoVector3D;
  v   : TGeoVector3D;
  n   : TGeoVector3D;
  dir : TGeoVector3D;
  w0  : TGeoVector3D;
  a   : TGeoFloat;
  b   : TGeoFloat;
  r   : TGeoFloat;
begin
  u.x := Triangle[2].x  - Triangle[1].x;
  u.y := Triangle[2].y  - Triangle[1].y;
  u.z := Triangle[2].z  - Triangle[1].z;

  v.x := Triangle[3].x  - Triangle[1].x;
  v.y := Triangle[3].y  - Triangle[1].y;
  v.z := Triangle[3].z  - Triangle[1].z;

  n   := Mul(u,v);

  dir.x := Line[2].x - Line[1].x;
  dir.y := Line[2].y - Line[1].y;
  dir.z := Line[2].z - Line[1].z;

  w0.x  := Line[1].x - Triangle[1].x;
  w0.y  := Line[1].y - Triangle[1].y;
  w0.z  := Line[1].z - Triangle[1].z;

  a := dotProduct(n,w0) * -1.0;
  b := dotProduct(n,dir);
  r := a / b;

  IPoint.x := Line[1].x + (r * dir.x);
  IPoint.y := Line[1].y + (r * dir.y);
  IPoint.z := Line[1].z + (r * dir.z);
end;
(* End of IntersectionPoint *)


procedure IntersectionPoint(const x1,y1,x2,y2,Cx,Cy,Radius:TGeoFloat; out ICnt:Integer; out Ix1,Iy1,Ix2,Iy2:TGeoFloat);
var
  Px   : TGeoFloat;
  Py   : TGeoFloat;
  S1In : Boolean;
  s2In : Boolean;
  h    : TGeoFloat;
  a    : TGeoFloat;
begin
  ICnt := 0;

  S1In := PointInCircle(x1,y1,Cx,Cy,Radius);
  S2In := PointInCircle(x2,y2,Cx,Cy,Radius);

  if S1In and S2In then
  begin
    ICnt := 2;
    Ix1  := x1;
    Iy1  := y1;
    Ix2  := x2;
    Iy2  := y2;
    Exit;
  end;

  if S1In Or S2In then
  begin
    ICnt := 2;
    ClosestPointOnLineFromPoint(x1,y1,x2,y2,Cx,Cy,Px,Py);
    if S1In then
    begin
      h   := Distance(Px,Py,Cx,Cy);
      a   := sqrt((Radius * Radius) - (h * h));
      Ix1 := x1;
      Iy1 := y1;
      ProjectPoint(Px,Py,x2,y2,a,Ix2,Iy2);
    end
    else if S2In then
    begin
      h   := Distance(Px,Py,Cx,Cy);
      a   := sqrt((Radius * Radius) - (h * h));
      Ix1 := x2;
      Iy1 := y2;
      ProjectPoint(Px,Py,x1,y1,a,Ix2,Iy2);
    end;
    Exit;
  end;

  ClosestPointOnSegmentFromPoint(x1,y1,x2,y2,Cx,Cy,Px,Py);

  if (IsEqual(x1,Px) and IsEqual(y1,Py)) or
     (IsEqual(x2,Px) and IsEqual(y2,Py)) then
    Exit
  else
  begin
    h := Distance(Px,Py,Cx,Cy);
    if h > Radius then
       Exit
    else if IsEqual(h,Radius) then
    begin
      ICnt := 1;
      Ix1  := Px;
      Iy1  := Py;
      Exit;
    end
    else if IsEqual(h,Zero) then
    begin
      ICnt := 2;
      ProjectPoint(Cx,Cy,x1,y1,Radius,Ix1,Iy1);
      ProjectPoint(Cx,Cy,x2,y2,Radius,Ix2,Iy2);
      Exit;
    end
    else
    begin
      ICnt := 2;
      a    := sqrt((Radius * Radius) - (h * h));
      ProjectPoint(Px,Py,x1,y1,a,Ix1,Iy1);
      ProjectPoint(Px,py,x2,y2,a,Ix2,Iy2);
      Exit;
    end;
  end;
end;
(* End Of IntersectionPoint *)


procedure IntersectionPoint(const Segment:TGeoSegment2D; const Circle:TGeoCircle; out ICnt:Integer; out I1,I2:TGeoPoint2D);
begin
  IntersectionPoint(
                    Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,
                    Circle.x,Circle.y,Circle.Radius,
                    ICnt,I1.x,I1.y,I2.x,I2.y
                   );
end;
(* End of IntersectionPoint *)


function NormalizeAngle(const Angle : TGeoFloat) : TGeoFloat;
begin
  Result := Angle;
  if Result > 360.0 then
    Result := Result - (Trunc(Result / 360.0) * 360.0)
  else if Result < 0 then
  begin
    while Result < Zero Do Result := Result + 360.0;
  end;
end;
(* Normalize Angle *)


function VerticalMirror(const Angle : TGeoFloat) : TGeoFloat;
begin
  Result := Angle;
  if IsEqual(Angle,  Zero) or
     IsEqual(Angle,180.0) or
     IsEqual(Angle,360.0) then Exit;
  Result := 360.0 - Result;
end;
(* Vertical Mirror *)


function HorizontalMirror(const Angle : TGeoFloat) : TGeoFloat;
begin
  Result := Angle;
  if Result <= 180.0 then
    Result := 180.0 - Result
  else
    Result := 540.0 - Result;
end;
(* Vertical Mirror *)


function Quadrant(const Angle : TGeoFloat  ):Integer;
begin
    Result := 0;
         if (Angle >=   0.0) and (Angle <  90.0) then Result := 1
    else if (Angle >=  90.0) and (Angle < 180.0) then Result := 2
    else if (Angle >= 180.0) and (Angle < 270.0) then Result := 3
    else if (Angle >= 270.0) and (Angle < 360.0) then Result := 4
    else if Angle = 360.0                      then Result := 1;
end;
(* End of Quadrant *)


function Quadrant(const x,y:TGeoFloat):Integer;
begin
  if      (x >  Zero) and (y >= Zero) then Result := 1
  else if (x <= Zero) and (y >  Zero) then Result := 2
  else if (x <  Zero) and (y <= Zero) then Result := 3
  else if (x >= Zero) and (y <  Zero) then Result := 4
  else
    Result := 0;
end;
(* End of Quadrant *)


function Quadrant(const Point : TGeoPoint2D):Integer;
begin
  Result := Quadrant(Point.x,Point.y);
end;
(* End of Quadrant *)


function VertexAngle(x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoFloat;
var
  Dist      : TGeoFloat;
  InputTerm : TGeoFloat;
begin
 (*
    Using the cosine identity:
    cosA = (b^2 + c^2 - a^2) / (2*b*c)
    A    = Cos'((b^2 + c^2 - a^2) / (2*b*c))

    Where:

    a,b and c : are edges in the triangle
    A         : is the angle at the vertex opposite edge 'a'
                aka the edge defined by the vertex <x1y1-x2y2-x3y3>

 *)
  (* Quantify coordinates *)
  x1   := x1 - x2;
  x3   := x3 - x2;
  y1   := y1 - y2;
  y3   := y3 - y2;

  (* Calculate Ley Distance *)
  Dist := (x1 * x1 + y1 * y1) * (x3 * x3 + y3 * y3);

  if IsEqual(Dist,Zero) then
    Result := Zero
  else
  begin
    InputTerm := (x1 * x3 + y1 * y3) / sqrt(Dist);
    if IsEqual(InputTerm,1.0) then
      Result := Zero
    else if IsEqual(InputTerm,-1.0) then
      Result := 180.0
    else
      Result := ArcCos(InputTerm) * _180DivPI
  end;
end;
(* End of Vertex Angle *)


function VertexAngle(const Point1,Point2,Point3:TGeoPoint2D):TGeoFloat;
begin
  Result := VertexAngle(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of Vertex Angle *)


function VertexAngle(x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):TGeoFloat;
var
  Dist : TGeoFloat;
begin
 (*
    Method is the same as the one described in
    the above routine.
 *)

  (* Quantify coordinates *)
  x1 := x1 - x2;
  x3 := x3 - x2;
  y1 := y1 - y2;
  y3 := y3 - y2;
  z1 := z1 - z2;
  z3 := z3 - z2;

  (* Calculate Ley Distance *)
  Dist := (x1 * x1 + y1 * y1 + z1 * z1) * (x3 * x3 + y3 * y3 + z3 * z3);

  if IsEqual(Dist,Zero) then
    Result := Zero
  else
    Result := ArcCos((x1 * x3+ y1 * y3 + z1 * z3) / sqrt(Dist)) * _180DivPI;
end;
(* End of Vertex Angle *)


function VertexAngle(const Point1,Point2,Point3:TGeoPoint3D):TGeoFloat;
begin
  Result := VertexAngle(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z);
end;
(* End of Vertex Angle *)


function OrientedVertexAngle(const x1,y1,x2,y2,x3,y3:TGeoFloat; const Orient : Integer = geoClockwise):TGeoFloat;
begin
  Result := VertexAngle(x1,y1,x2,y2,x3,y3);
  if Orientation(x1,y1,x2,y2,x3,y3) <> Orient then
    Result := 360.0 - Result;
end;
(* End of Oriented Vertex Angle *)


function OrientedVertexAngle(const Point1,Point2,Point3:TGeoPoint2D; const Orient : Integer = geoClockwise):TGeoFloat;
begin
  Result := OrientedVertexAngle(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Orient);
end;
(* End of Oriented Vertex Angle *)


function CartesianAngle(const x,y:TGeoFloat):TGeoFloat;
begin
  if      (x >  Zero) and (y >  Zero) then Result := (ArcTan( y / x) * _180DivPI)
  else if (x <  Zero) and (y >  Zero) then Result := (ArcTan(-x / y) * _180DivPI) +  90.0
  else if (x <  Zero) and (y <  Zero) then Result := (ArcTan( y / x) * _180DivPI) + 180.0
  else if (x >  Zero) and (y <  Zero) then Result := (ArcTan(-x / y) * _180DivPI) + 270.0
  else if (x  = Zero) and (y >  Zero) then Result :=  90.0
  else if (x <  Zero) and (y =  Zero) then Result := 180.0
  else if (x  = Zero) and (y <  Zero) then Result := 270.0
  else
    Result := Zero;
end;
(* End of Cartesian Angle *)


function CartesianAngle(const Point : TGeoPoint2D):TGeoFloat;
begin
  Result := CartesianAngle(Point.x,Point.y);
end;
(* End of Cartesian Angle *)


function RobustCartesianAngle(const x,y:TGeoFloat; const Epsilon : TGeoFloat = Epsilon_High):TGeoFloat;
begin
  if             (x >  Zero)         and        (y >  Zero)         then Result := (ArcTan( y / x) * _180DivPI)
  else if        (x <  Zero)         and        (y >  Zero)         then Result := (ArcTan(-x / y) * _180DivPI) +  90.0
  else if        (x <  Zero)         and        (y <  Zero)         then Result := (ArcTan( y / x) * _180DivPI) + 180.0
  else if        (x >  Zero)         and        (y <  Zero)         then Result := (ArcTan(-x / y) * _180DivPI) + 270.0
  else if IsEqual(x,   Zero,Epsilon) and        (y >  Zero)         then Result :=  90.0
  else if        (x <  Zero)         and IsEqual(y,   Zero,Epsilon) then Result := 180.0
  else if IsEqual(x,   Zero)         and        (y <  Zero)         then Result := 270.0
  else
    Result := Zero;
end;
(* End of Robust Cartesian Angle *)


function RobustCartesianAngle(const Point : TGeoPoint2D; const Epsilon : TGeoFloat = Epsilon_High):TGeoFloat;
begin
  Result := RobustCartesianAngle(Point.x,Point.y,Epsilon);
end;
(* End of Robust Cartesian Angle *)


function SegmentIntersectAngle(const Point1,Point2,Point3,Point4:TGeoPoint2D):TGeoFloat;
var
  TempPoint : TGeoPoint2D;
begin
  Result := -1;
  if Intersect(Point1,Point2,Point3,Point4) then
  begin
    TempPoint := IntersectionPoint(Point1,Point2,Point3,Point4);
    Result    := VertexAngle(Point1,TempPoint,Point4);
  end;
end;
(* End of SegmentIntersectAngle *)


function SegmentIntersectAngle(const Segment1,Segment2:TGeoSegment2D):TGeoFloat;
var
  TempPoint : TGeoPoint2D;
begin
  Result := -1;
  if Intersect(Segment1,Segment2) then
  begin
    TempPoint := IntersectionPoint(Segment1,Segment2);
    Result    := VertexAngle(Segment1[1],TempPoint,Segment2[1]);
  end;
end;
(* End of SegmentIntersectAngle *)


function SegmentIntersectAngle(const Point1,Point2,Point3,Point4:TGeoPoint3D):TGeoFloat;
begin
 {
  This section can be completed once line intersection in 3D is complete
 }
  Result := Zero;
end;
(* End of SegmentIntersectAngle *)


function SegmentIntersectAngle(const Segment1,Segment2:TGeoSegment3D):TGeoFloat;
begin
 {
  This section can be completed once line intersection in 3D is complete
 }
  Result := Zero;
end;
(* End of SegmentIntersectAngle *)


function InPortal(const P:TGeoPoint2D):Boolean;
begin
  Result := PointInRectangle(P,MinimumX,MinimumY,MaximumX,MaximumY);
end;
(* End of InPortal *)


function InPortal(const P:TGeoPoint3D):Boolean;
begin
  Result := LessThanOrEqual(MinimumX,P.x) and LessThanOrEqual(MaximumZ,P.x) and
            LessThanOrEqual(MinimumY,P.y) and LessThanOrEqual(MaximumY,P.y) and
            LessThanOrEqual(MinimumZ,P.y) and LessThanOrEqual(MaximumZ,P.y);
end;
(* End of InPortal *)


function HighestPoint(const Polygon:TGeoPolygon2D):TGeoPoint2D;
var
  i : Integer;
begin
  if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
    if Polygon[i].y > Result.y then
    Result := Polygon[i];
end;
(* End of HighestPoint *)


function HighestPoint(const Point: array of TGeoPoint2D):TGeoPoint2D;
var
  i : Integer;
begin
  if Length(Point) = 0 then Exit;
  Result := Point[0];
  for i := 1 to Length(Point) - 1 do
    if Point[i].y > Result.y then
      Result := Point[i];
end;
(* End of HighestPoint *)


function HighestPoint(const Triangle:TGeoTriangle2D):TGeoPoint2D;
begin
  Result := Triangle[1];
  if Triangle[2].y > Result.y then
    Result := Triangle[2];
  if Triangle[3].y > Result.y then
    Result := Triangle[3];
end;
(* End of HighestPoint *)


function HighestPoint(const Triangle:TGeoTriangle3D):TGeoPoint3D;
begin
  Result := Triangle[1];
  if Triangle[2].y > Result.y then
    Result := Triangle[2];
  if Triangle[3].y > Result.y then
    Result := Triangle[3];
end;
(* End of HighestPoint *)


function HighestPoint(const Quadix:TQuadix2D):TGeoPoint2D;
begin
  Result := Quadix[1];
  if Quadix[2].y > Result.y then
    Result := Quadix[2];
  if Quadix[3].y > Result.y then
    Result := Quadix[3];
  if Quadix[4].y > Result.y then
    Result := Quadix[4];
end;
(* End of HighestPoint *)


function HighestPoint(const Quadix:TQuadix3D):TGeoPoint3D;
begin
  Result := Quadix[1];
  if Quadix[2].y > Result.y then
    Result := Quadix[2];
  if Quadix[3].y > Result.y then
    Result := Quadix[3];
  if Quadix[4].y > Result.y then
    Result := Quadix[4];
end;
(* End of HighestPoint *)


function LowestPoint(const Polygon: TGeoPolygon2D):TGeoPoint2D;
var
  i : Integer;
begin
  if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
    if Polygon[i].y < Result.y then
      Result := Polygon[i];
end;
(* End of LowestPoint *)


function LowestPoint(const Point: array of TGeoPoint2D):TGeoPoint2D;
var
  i : Integer;
begin
  if Length(Point) = 0 then Exit;
  Result := Point[0];
  for i := 1 to length(Point) - 1 do
   if Point[i].y < Result.y then
     Result := Point[i];
end;
(* End of LowestPoint *)


function LowestPoint(const Triangle:TGeoTriangle2D):TGeoPoint2D;
begin
  Result := Triangle[1];
  if Triangle[2].y < Result.y then
     Result := Triangle[2];
  if Triangle[3].y < Result.y then
     Result := Triangle[3];
end;
(* End of LowestPoint *)


function LowestPoint(const Triangle:TGeoTriangle3D):TGeoPoint3D;
begin
  Result := Triangle[1];
  if Triangle[2].y < Result.y then
     Result := Triangle[2];
  if Triangle[3].y < Result.y then
     Result := Triangle[3];
end;
(* End of LowestPoint *)


function LowestPoint(const Quadix:TQuadix2D):TGeoPoint2D;
begin
  Result := Quadix[1];
  if Quadix[2].y < Result.y then
     Result := Quadix[2];
  if Quadix[3].y < Result.y then
     Result := Quadix[3];
  if Quadix[4].y < Result.y then
     Result := Quadix[4];
end;
(* End of LowestPoint *)


function LowestPoint(const Quadix:TQuadix3D):TGeoPoint3D;
begin
  Result := Quadix[1];
  if Quadix[2].y < Result.y then
     Result := Quadix[2];
  if Quadix[3].y < Result.y then
     Result := Quadix[3];
  if Quadix[4].y < Result.y then
     Result := Quadix[4];
end;
(* End of LowestPoint *)


function MostLeftPoint(const Polygon: TGeoPolygon2D):TGeoPoint2D;
var
  i : Integer;
begin
  if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
   if Polygon[i].x < Result.x then
     Result := Polygon[i];
end;
(* End of Most Left Point *)


function MostLeftPoint(const Point: array of TGeoPoint2D):TGeoPoint2D;
begin
  Result := MostLeftPoint(TGeoPolygon2D(@Point));
end;
(* End of Most Left Point *)


function MostRightPoint(const Polygon: TGeoPolygon2D):TGeoPoint2D;
var
  i : Integer;
begin
  if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
    if Polygon[i].x > Result.x then
      Result := Polygon[i];
end;
(* End of Most Right Point *)


function MostRightPoint(const Point: array of TGeoPoint2D):TGeoPoint2D;
begin
  Result := MostRightPoint(TGeoPolygon2D(@Point));
end;
(* End of Most Right Point *)


function MostUpperRight(const Polygon: TGeoPolygon2D):TGeoPoint2D;
var
  i : Integer;
begin
   if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
    if Polygon[i].y > Result.y then
      Result := Polygon[i]
    else if Polygon[i].y = Result.y then
      if Polygon[i].x > Result.x then
        Result := Polygon[i];
end;
(* End of Most Upper Right *)


function MostUpperRight(const Point: array of TGeoPoint2D):TGeoPoint2D;
begin
  Result := MostUpperRight(TGeoPolygon2D(@Point));
end;
(* End of Most Upper Right *)


function MostUpperLeft(const Polygon: TGeoPolygon2D):TGeoPoint2D;
var
  i : Integer;
begin
  if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
   if Polygon[i].y > Result.y then
     Result := Polygon[i]
   else if Polygon[i].y = Result.y then
    if Polygon[i].x < Result.x then
      Result := Polygon[i];
end;
(* End of Most Upper Left *)


function MostUpperLeft(const Point: array of TGeoPoint2D):TGeoPoint2D;
begin
 Result := MostUpperLeft(TGeoPolygon2D(@Point));
end;
(* End of Most Upper Left *)


function MostLowerRight(const Polygon: TGeoPolygon2D):TGeoPoint2D;
var
  i : Integer;
begin
 if Length(Polygon) = 0 then Exit;
 Result := Polygon[0];
 for i := 1 to Length(Polygon) - 1 do
  if Polygon[i].y < Result.y then
    Result := Polygon[i]
  else if Polygon[i].y = Result.y then
   if Polygon[i].x > Result.x then
     Result := Polygon[i];
end;
(* End of Most Lower Right *)


function MostLowerRight(const Point: array of TGeoPoint2D):TGeoPoint2D;
begin
 Result := MostLowerRight(TGeoPolygon2D(@Point));
end;
(* End of Most Lower Right *)


function MostLowerLeft(const Polygon: TGeoPolygon2D):TGeoPoint2D;
var
  i : Integer;
begin
 if Length(Polygon) = 0 then Exit;
 Result := Polygon[0];
 for i := 1 to Length(Polygon) - 1 do
  if Polygon[i].y < Result.y then
    Result := Polygon[i]
  else if Polygon[i].y = Result.y then
   if Polygon[i].x < Result.x then
     Result := Polygon[i];
end;
(* End of Most Lower Left *)


function MostLowerLeft(const Point: array of TGeoPoint2D):TGeoPoint2D;
begin
  Result := MostLowerLeft(TGeoPolygon2D(@Point));
end;
(* End of Most Lower Left *)


function GeoMin(const Point1,Point2:TGeoPoint2D):TGeoPoint2D;
begin
       if Point1.x < Point2.x then Result := Point1
  else if Point2.x < Point1.x then Result := Point2
  else if Point1.y < Point2.y then Result := Point1
  else                             Result := Point2;
end;
(* End of Minimum Between 2 Points *)


function GeoMin(const Point1,Point2:TGeoPoint3D):TGeoPoint3D;
begin
       if Point1.x < Point2.x then Result := Point1
  else if Point2.x < Point1.x then Result := Point2
  else if Point1.y < Point2.y then Result := Point1
  else if Point2.y < Point1.y then Result := Point2
  else if Point1.z < Point2.z then Result := Point1
  else                             Result := Point2;
end;
(* End of Minimum Between 2 Points *)


function GeoMax(const Point1,Point2:TGeoPoint2D):TGeoPoint2D;
begin
       if Point1.x > Point2.x then Result := Point1
  else if Point2.x > Point1.x then Result := Point2
  else if Point1.y > Point2.y then Result := Point1
  else                             Result := Point2;
end;
(* End of Maximum Between 2 Points *)


function GeoMax(const Point1,Point2:TGeoPoint3D):TGeoPoint3D;
begin
       if Point1.x > Point2.x then Result := Point1
  else if Point2.x > Point1.x then Result := Point2
  else if Point1.y > Point2.y then Result := Point1
  else if Point2.y > Point1.y then Result := Point2
  else if Point1.z > Point2.z then Result := Point1
  else                             Result := Point2;
end;
(* End of Maximum Between 2 Points *)


function Coincident(const Point1,Point2:TGeoPoint2D):Boolean;
begin
  Result := IsEqual(Point1,Point2);
end;
(* End of Coincident - 2D Points *)


function Coincident(const Point1,Point2:TGeoPoint3D):Boolean;
begin
  Result := IsEqual(Point1,Point2);
end;
(* End of Coincident - 3D Points *)


function Coincident(const Segment1,Segment2:TGeoSegment2D):Boolean;
begin
  Result := (Coincident(Segment1[1],Segment2[1]) and Coincident(Segment1[2],Segment2[2])) or
            (Coincident(Segment1[1],Segment2[2]) and Coincident(Segment1[2],Segment2[1]));
end;
(* End of Coincident - 2D Segments *)


function Coincident(const Segment1,Segment2:TGeoSegment3D):Boolean;
begin
  Result := (Coincident(Segment1[1],Segment2[1]) and  Coincident(Segment1[2],Segment2[2])) or
            (Coincident(Segment1[1],Segment2[2]) and  Coincident(Segment1[2],Segment2[1]));
end;
(* End of Coincident - 3D Segments *)


function Coincident(const Triangle1,Triangle2:TGeoTriangle2D):Boolean;
var
  Flag  : array [1..3] of Boolean;
  Count : Integer;
  i     : Integer;
  j     : Integer;
begin
  Count := 0;
  for i := 1 to 3 do Flag[i] := False;
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
    if not Flag[i] then
      if Coincident(Triangle1[i],Triangle2[j]) then
      begin
        Inc(Count);
        Flag[i] := True;
        Break;
      end;
  end;
  Result := (Count = 3);
end;
(* End of Coincident - 2D Triangles *)


function Coincident(const Triangle1,Triangle2:TGeoTriangle3D):Boolean;
var
  Flag  : array [1..3] of Boolean;
  Count : Integer;
  i     : Integer;
  j     : Integer;
begin
  Count := 0;
  for i := 1 to 3 do Flag[i] := False;
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
    if not Flag[i] then
      if Coincident(Triangle1[i],Triangle2[j]) then
      begin
        Inc(Count);
        Flag[i] := True;
        Break;
      end;
  end;
  Result := (Count = 3);
end;
(* End of Coincident - 3D Triangles *)


function Coincident(const Rect1,Rect2:TGeoRectangle):Boolean;
begin
  Result := Coincident(Rect1[1],Rect2[1]) and
            Coincident(Rect1[2],Rect2[2]);
end;
(* End of Coincident - Rectangles *)


function Coincident(const Quad1,Quad2:TQuadix2D):Boolean;
var
  Flag  : array [1..4] of Boolean;
  Count : Integer;
  i     : Integer;
  j     : Integer;
begin
  Result := False;
  if ConvexQuadix(Quad1) <> ConvexQuadix(Quad2) then Exit;
  Count := 0;
  for i := 1 to 4 do Flag[I] := False;
  for i := 1 to 4 do
  begin
    for j := 1 to 4 do
    if not Flag[i] then
      if Coincident(Quad1[i],Quad2[j]) then
      begin
        Inc(Count);
        Flag[i] := True;
        Break;
      end;
  end;
  Result := (Count = 4);
end;
(* End of Coincident - 2D Quadii *)


function Coincident(const Quad1,Quad2:TQuadix3D):Boolean;
begin
  (* to be implemented at a later date *)
  Result := False;
end;
(* End of Coincident - 3D Quadii *)


function Coincident(const Circle1,Circle2:TGeoCircle):Boolean;
begin
 Result := IsEqual(Circle1.x      , Circle2.x) and
           IsEqual(Circle1.y      , Circle2.y) and
           IsEqual(Circle1.Radius , Circle2.Radius);
end;
(* End of Coincident - Circles *)


function Coincident(const Sphr1,Sphr2:TGeoSphere):Boolean;
begin
  Result := IsEqual(Sphr1.x      , Sphr2.x) and
            IsEqual(Sphr1.y      , Sphr2.y) and
            IsEqual(Sphr1.z      , Sphr2.z) and
            IsEqual(Sphr1.Radius , Sphr2.Radius);
end;
(* End of Coincident - Spheres *)


function Coincident(const Obj1,Obj2:TGeometricObject):Boolean;
begin
  if (Obj1.ObjectType = geoPoint2D) and (Obj2.ObjectType = geoPoint2D) then
    Result := Coincident(Obj1.Point2D,Obj2.Point2D)
  else if (Obj1.ObjectType = geoPoint3D) and (Obj2.ObjectType = geoPoint3D) then
    Result := Coincident(Obj1.Point3D,Obj2.Point3D)
  else if (Obj1.ObjectType = geoSegment2D) and (Obj2.ObjectType = geoSegment2D) then
    Result := Coincident(Obj1.Segment2D,Obj2.Segment2D)
  else if (Obj1.ObjectType = geoSegment3D) and (Obj2.ObjectType = geoSegment3D) then
    Result := Coincident(Obj1.Segment3D,Obj2.Segment3D)
  else if (Obj1.ObjectType = geoTriangle2D) and (Obj2.ObjectType = geoTriangle2D) then
    Result := Coincident(Obj1.Triangle2D,Obj2.Triangle2D)
  else if (Obj1.ObjectType = geoTriangle3D) and (Obj2.ObjectType = geoTriangle3D) then
    Result := Coincident(Obj1.Triangle3D,Obj2.Triangle3D)
  else if (Obj1.ObjectType = geoQuadix2D) and (Obj2.ObjectType = geoQuadix2D) then
    Result := Coincident(Obj1.Quadix2D,Obj2.Quadix2D)
  else if (Obj1.ObjectType = geoQuadix3D) and (Obj2.ObjectType = geoQuadix3D) then
    Result := Coincident(Obj1.Quadix3D,Obj2.Quadix3D)
  else if (Obj1.ObjectType = geoCircle) and (Obj2.ObjectType = geoCircle) then
    Result := Coincident(Obj1.Circle,Obj2.Circle)
  else if (Obj1.ObjectType = geoSphere) and (Obj2.ObjectType = geoSphere) then
    Result := Coincident(Obj1.Sphere,Obj2.Sphere)
  else
    Result := False;
end;
(* End of Coincident - Geometric Object *)


function Parallel(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := IsEqual(((y1 - y2) * (x3 - x4)),((y3 - y4) * (x1 - x2)),Epsilon);
end;
(* End of Parallel *)


function Parallel(const Point1,Point2,Point3,Point4:TGeoPoint2D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := Parallel(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y,Epsilon);
end;
(* End of Parallel *)


function Parallel(const Segment1,Segment2:TGeoSegment2D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := Parallel(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Parallel *)


function Parallel(const Line1,Line2:TGeoLine2D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := Parallel(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Parallel *)


function Parallel(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
var
  Dx1 : TGeoFloat;
  Dx2 : TGeoFloat;
  Dy1 : TGeoFloat;
  Dy2 : TGeoFloat;
  Dz1 : TGeoFloat;
  Dz2 : TGeoFloat;
begin
 (*
    Theory:
    if the gradients in the following planes x-y, y-z, z-x are equal then
    it can be said that the segments are parallel in 3D - ~ i think ~
    Worst case scenario: 6 floating point multiplications and 9 floating
    point subtractions
 *)

  Result := False;

  Dx1 := x1 - x2;
  Dx2 := x3 - x4;

  Dy1 := y1 - y2;
  Dy2 := y3 - y4;

  Dz1 := z1 - z2;
  Dz2 := z3 - z4;

  if NotEqual((Dy1 * Dx2),(Dy2 * Dx1),Epsilon) then Exit;
  if NotEqual((Dz1 * Dy2),(Dz2 * Dy1),Epsilon) then Exit;
  if NotEqual((Dx1 * Dz2),(Dx2 * Dz1),Epsilon) then Exit;

  Result := True;
end;
(* End of Parallel *)


function Parallel(const Point1,Point2,Point3,Point4:TGeoPoint3D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result:= Parallel(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z,Epsilon)
end;
(* End of Parallel *)


function Parallel(const Segment1,Segment2:TGeoSegment3D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result:= Parallel(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Parallel *)


function Parallel(const Line1,Line2:TGeoLine3D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result:= Parallel(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Parallel *)


function RobustParallel(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
var
   Px1 : TGeoFloat;
   Py1 : TGeoFloat;
   Px2 : TGeoFloat;
   Py2 : TGeoFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,x3,y3,Px1,Py1);
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,x4,y4,Px2,Py2);
  Result := IsEqual(Distance(x3,y3,Px1,Py1),Distance(x4,y4,Px2,Py2),Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Point1,Point2,Point3,Point4:TGeoPoint2D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y,Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Segment1,Segment2:TGeoSegment2D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Line1,Line2:TGeoLine2D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat;const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
var
  Px1 : TGeoFloat;
  Py1 : TGeoFloat;
  Pz1 : TGeoFloat;
  Px2 : TGeoFloat;
  Py2 : TGeoFloat;
  Pz2 : TGeoFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,x3,y3,z3,Px1,Py1,Pz1);
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,x4,y4,z4,Px2,Py2,Pz2);
  Result := IsEqual(Distance(x3,y3,z3,Px1,Py1,Pz1),Distance(x4,y4,z4,Px2,Py2,Pz2),Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Point1,Point2,Point3,Point4:TGeoPoint3D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point3.z,Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Segment1,Segment2:TGeoSegment3D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Line1,Line2:TGeoLine3D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Robust Parallel *)


function Perpendicular(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result:= IsEqual(-1.0 * (y2 - y1) * (y4 - y3),(x4 - x3) * (x2 - x1), Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const Point1,Point2,Point3,Point4:TGeoPoint2D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := Perpendicular(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y,Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const Segment1,Segment2:TGeoSegment2D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result:= Perpendicular(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const Line1,Line2:TGeoLine2D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result:= Perpendicular(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
var
  Dx1 : TGeoFloat;
  Dx2 : TGeoFloat;
  Dy1 : TGeoFloat;
  Dy2 : TGeoFloat;
  Dz1 : TGeoFloat;
  Dz2 : TGeoFloat;
begin
 (*
    The dot product of the vector forms of the segments will
    be 0 if the segments are perpendicular
 *)

  Dx1 := x1 - x2;
  Dx2 := x3 - x4;

  Dy1 := y1 - y2;
  Dy2 := y3 - y4;

  Dz1 := z1 - z2;
  Dz2 := z3 - z4;

  Result := IsEqual((Dx1 * Dx2) + (Dy1 * Dy2) + (Dz1 * Dz2),Zero,Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const Point1,Point2,Point3,Point4:TGeoPoint3D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := Perpendicular(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z);
end;
(* End of Perpendicular *)


function Perpendicular(const Segment1,Segment2:TGeoSegment3D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := Perpendicular(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const Line1,Line2:TGeoLine3D; const Epsilon : TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := Perpendicular(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Perpendicular *)


function RobustPerpendicular(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
var
  P1x : TGeoFloat;
  P1y : TGeoFloat;
  P2x : TGeoFloat;
  P2y : TGeoFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,x3,y3,P1x,P1y);
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,x4,y4,P2x,P2y);
  Result := IsEqual(Distance(P1x,P1y,P2x,P2y),Zero,Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Point1,Point2,Point3,Point4:TGeoPoint2D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y,Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Segment1,Segment2:TGeoSegment2D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Line1,Line2:TGeoLine2D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
var
  P1x : TGeoFloat;
  P1y : TGeoFloat;
  P1z : TGeoFloat;
  P2x : TGeoFloat;
  P2y : TGeoFloat;
  P2z : TGeoFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,x3,y3,z3,P1x,P1y,P1z);
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,x4,y4,z3,P2x,P2y,P2z);
  Result := IsEqual(Distance(P1x,P1y,P1z,P2x,P2y,P2z),Zero,Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Point1,Point2,Point3,Point4:TGeoPoint3D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z,Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Segment1,Segment2:TGeoSegment3D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Line1,Line2:TGeoLine3D; const Epsilon:TGeoFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Robust Perpendicular *)


function LineToLineIntersect(x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;
begin
  Result := False;
  if NotEqual((x1 - x2) * (y3 - y4),(y1 - y2) * (x3 - x4)) then
    Result := True
  else if Collinear(x1,y1,x2,y2,x3,y3) then
    Result := True;
end;
(* End of LineToLineIntersect *)


function LineToLineIntersect(Line1,Line2:TGeoLine2D):Boolean;
begin
  Result := LineToLineIntersect(Line1[1].x,Line1[1].y,Line1[2].x,Line1[2].y,Line2[1].x,Line2[1].y,Line2[2].x,Line2[2].y);
end;
(* End of LineToLineIntersect *)


function RectangleToRectangleIntersect(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;
begin
  (*
    Assumes that:  x1 < x2, y1 < y2, x3 < x4, y3 < y4
  *)
  Result := (x1 <= x4) and (x2 >= x3) and (y1 <= y4) and (y2 >= y3);
end;
(* End of Rectangle To Rectangle Intersect *)


function RectangleToRectangleIntersect(const Rectangle1,Rectangle2:TGeoRectangle):Boolean;
begin
  Result := RectangleToRectangleIntersect(
                                          Rectangle1[1].x,Rectangle1[1].y,Rectangle1[2].x,Rectangle1[2].y,
                                          Rectangle2[1].x,Rectangle2[1].y,Rectangle2[2].x,Rectangle2[2].y
                                         );
end;
(* End of Rectangle To Rectangle Intersect *)


function RectangleWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;
begin
  Result := PointInRectangle(x1,y1,x3,y3,x4,y4) and PointInRectangle(x2,y2,x3,y3,x4,y4);
end;
(* End of Rectangle Within Rectangle Intersect *)


function RectangleWithinRectangle(const Rectangle1,Rectangle2:TGeoRectangle):Boolean;
begin
  Result := RectangleWithinRectangle(
                                     Rectangle1[1].x,Rectangle1[1].y,Rectangle1[2].x,Rectangle1[2].y,
                                     Rectangle2[1].x,Rectangle2[1].y,Rectangle2[2].x,Rectangle2[2].y
                                    );
end;
(* End of Rectangle Within Rectangle Intersect *)


function CircleWithinRectangle(const x,y,Radius,x1,y1,x2,y2:TGeoFloat):Boolean;
begin
  Result := RectangleWithinRectangle(AABB(EquateCircle(x,y,Radius)),EquateRectangle(x1,y1,x2,y2));
end;
(* End of Circle Within Rectangle Intersect *)


function CircleWithinRectangle(const Circle:TGeoCircle; const Rectangle:TGeoRectangle):Boolean;
begin
  Result := RectangleWithinRectangle(AABB(Circle),Rectangle);
end;
(* End of Circle Within Rectangle Intersect *)


function TriangleWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4,x5,y5:TGeoFloat):Boolean;
begin
  Result := PointInRectangle(x1,y1,x4,y4,x5,y5) and
            PointInRectangle(x2,y2,x4,y4,x5,y5) and
            PointInRectangle(x3,y3,x4,y4,x5,y5);
end;
(* End of Triangle Within Rectangle *)


function TriangleWithinRectangle(const Triangle:TGeoTriangle2D; const Rectangle:TGeoRectangle):Boolean;
begin
  Result := PointInRectangle(Triangle[1],Rectangle) and
            PointInRectangle(Triangle[2],Rectangle) and
            PointInRectangle(Triangle[3],Rectangle);
end;
(* End of Triangle Within Rectangle *)


function SegmentWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;
begin
  Result := PointInRectangle(x1,y1,x3,y3,x4,y4) and
            PointInRectangle(x2,y2,x3,y3,x4,y4);
end;
(* End of Segment Within Rectangle *)


function SegmentWithinRectangle(const Segment:TGeoSegment2D; const Rectangle:TGeoRectangle):Boolean;
begin
  Result := SegmentWithinRectangle(  Segment[1].x,  Segment[1].y,  Segment[2].x,  Segment[2].y,
                                   Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
end;
(* End of Segment Within Rectangle *)


function CircleInCircle(const Circle1,Circle2:TGeoCircle):Boolean;
begin
  Result := (Circle2.Radius - (Distance(Circle1.x,Circle1.y,Circle2.x,Circle2.y) + Circle1.Radius) >= Zero);
end;
(* End of CircleInCircle *)


function IsTangent(const Segment:TGeoSegment2D; const Circle:TGeoCircle):Boolean;
var
  rSqr        : TGeoFloat;
  drSqr       : TGeoFloat;
  dSqr        : TGeoFloat;
  TempSegment : TGeoSegment2D;
begin
  TempSegment := Translate(-Circle.x,-Circle.y,Segment);
  rSqr        := Circle.Radius * Circle.Radius;
  drSqr       := LayDistance(TempSegment);
  dSqr        := Sqr(TempSegment[1].x * TempSegment[2].y - TempSegment[2].x * TempSegment[1].y);
  Result      := IsEqual((rSqr * drSqr - dSqr),Zero);
end;
(* End of IsTangent *)


function PointOfReflection(const Sx1,Sy1,Sx2,Sy2,P1x,P1y,P2x,P2y:TGeoFloat; out RPx,RPy:TGeoFloat):Boolean;
var
  Ix   : TGeoFloat;
  Iy   : TGeoFloat;
  P1Px : TGeoFloat;
  P1Py : TGeoFloat;
  P2Px : TGeoFloat;
  P2Py : TGeoFloat;
begin
  Result := False;
  if (not Collinear(Sx1,Sy1,Sx2,Sy2,P1x,P1y)) and
     (not Collinear(Sx1,Sy1,Sx2,Sy2,P2x,P2y)) then
  begin
    ClosestPointOnLineFromPoint(Sx1,Sy1,Sx2,Sy2,P1x,P1y,P1Px,P1Py);
    ClosestPointOnLineFromPoint(Sx1,Sy1,Sx2,Sy2,P2x,P2y,P2Px,P2Py);
    Intersect(P1x,P1y,P2Px,P2Py,P2x,P2y,P1Px,P1Py,Ix,Iy);
    ClosestPointOnLineFromPoint(Sx1,Sy1,Sx2,Sy2,Ix,Iy,RPx,RPy);
    if IsPointCollinear(Sx1,Sy1,Sx2,Sy2,RPx,RPy) then
    begin
      Result := True
    end;
  end;
end;
(* End of PointOfReflection *)


function PointOfReflection(const Segment:TGeoSegment2D; const P1,P2:TGeoPoint2D; out RP:TGeoPoint2D):Boolean;
begin
  Result := PointOfReflection(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,P1.x,P1.y,P2.x,P2.y,RP.x,RP.y);
end;
(* End of PointOfReflection *)


procedure Mirror(const Px,Py,x1,y1,x2,y2:TGeoFloat; out Nx,Ny:TGeoFloat);
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
  Nx := Px + 2 * (Nx - Px);
  Ny := Py + 2 * (Ny - Py);
end;
(* End of Mirror *)


function Mirror(const Point:TGeoPoint2D; const Line:TGeoLine2D):TGeoPoint2D;
begin
  Mirror(Point.x,Point.y,Line[1].x,Line[1].y,Line[2].x,Line[2].y,Result.x,Result.y);
end;
(* End of Mirror *)


function Mirror(const Segment:TGeoSegment2D; const Line:TGeoLine2D):TGeoSegment2D;
begin
  Result[1] := Mirror(Segment[1],Line);
  Result[2] := Mirror(Segment[2],Line);
end;
(* End of Mirror *)


function Mirror(const Rectangle:TGeoRectangle; const Line:TGeoLine2D):TGeoRectangle;
begin
  Result[1] := Mirror(Rectangle[1],Line);
  Result[2] := Mirror(Rectangle[2],Line);
end;
(* End of Mirror *)


function Mirror(const Triangle:TGeoTriangle2D; const Line:TGeoLine2D):TGeoTriangle2D;
begin
  Result[1] := Mirror(Triangle[1],Line);
  Result[2] := Mirror(Triangle[2],Line);
  Result[3] := Mirror(Triangle[3],Line);
end;
(* End of Mirror *)


function Mirror(const Quadix:TQuadix2D; const Line:TGeoLine2D):TQuadix2D;
begin
  Result[1] := Mirror(Quadix[1],Line);
  Result[2] := Mirror(Quadix[2],Line);
  Result[3] := Mirror(Quadix[3],Line);
end;
(* End of Mirror *)


function Mirror(const Circle:TGeoCircle; const Line:TGeoLine2D):TGeoCircle;
begin
  Result.Radius := Circle.Radius;
  Mirror(Circle.x,Circle.y,Line[1].x,Line[1].y,Line[2].x,Line[2].y,Result.x,Result.y);
end;
(* End of Mirror *)


function Mirror(const Obj:TGeometricObject; const Line:TGeoLine2D):TGeometricObject;
begin
  Result.ObjectType := Obj.ObjectType;
  case Obj.ObjectType of
    geoSegment2D  : Result.Segment2D  := Mirror(Obj.Segment2D ,Line);
    geoTriangle2D : Result.Triangle2D := Mirror(Obj.Triangle2D,Line);
    geoRectangle  : Result.Rectangle  := Mirror(Obj.Rectangle ,Line);
    geoQuadix2D   : Result.Quadix2D   := Mirror(Obj.Quadix2D  ,Line);
    geoCircle     : Result.Circle     := Mirror(Obj.Circle    ,Line);
  end;
end;
(* End of Mirror *)


procedure NonSymmetricMirror(const Px,Py,x1,y1,x2,y2:TGeoFloat; const Ratio:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  GeneralRatio : TGeoFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
  GeneralRatio := 2 * Ratio;
  Nx := Px + GeneralRatio * (Nx - Px);
  Ny := Py + GeneralRatio * (Ny - Py);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Point:TGeoPoint2D; const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeoPoint2D;
begin
  NonSymmetricMirror(Point.x,Point.y,Ratio,Line[1].x,Line[1].y,Line[2].x,Line[2].y,Result.x,Result.y);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Segment:TGeoSegment2D; const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeoSegment2D;
begin
  Result[1] := NonSymmetricMirror(Segment[1],Ratio,Line);
  Result[2] := NonSymmetricMirror(Segment[2],Ratio,Line);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Rectangle:TGeoRectangle; const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeoRectangle;
begin
  Result[1] := NonSymmetricMirror(Rectangle[1],Ratio,Line);
  Result[2] := NonSymmetricMirror(Rectangle[2],Ratio,Line);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Triangle:TGeoTriangle2D; const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeoTriangle2D;
begin
  Result[1] := NonSymmetricMirror(Triangle[1],Ratio,Line);
  Result[2] := NonSymmetricMirror(Triangle[2],Ratio,Line);
  Result[3] := NonSymmetricMirror(Triangle[3],Ratio,Line);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Quadix:TQuadix2D; const Ratio:TGeoFloat; const Line:TGeoLine2D):TQuadix2D;
begin
  Result[1] := NonSymmetricMirror(Quadix[1],Ratio,Line);
  Result[2] := NonSymmetricMirror(Quadix[2],Ratio,Line);
  Result[3] := NonSymmetricMirror(Quadix[3],Ratio,Line);
  Result[4] := NonSymmetricMirror(Quadix[4],Ratio,Line);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Circle:TGeoCircle; const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeoCircle;
begin
  Result.Radius := Circle.Radius;
  NonSymmetricMirror(Circle.x,Circle.y,Ratio,Line[1].x,Line[1].y,Line[2].x,Line[2].y,Result.x,Result.y);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Obj:TGeometricObject; const Ratio:TGeoFloat; const Line:TGeoLine2D):TGeometricObject;
begin
  Result.ObjectType := Obj.ObjectType;
  case Obj.ObjectType of
    geoSegment2D  : Result.Segment2D  := NonSymmetricMirror(Obj.Segment2D ,Ratio,Line);
    geoTriangle2D : Result.Triangle2D := NonSymmetricMirror(Obj.Triangle2D,Ratio,Line);
    geoRectangle  : Result.Rectangle  := NonSymmetricMirror(Obj.Rectangle ,Ratio,Line);
    geoQuadix2D   : Result.Quadix2D   := NonSymmetricMirror(Obj.Quadix2D  ,Ratio,Line);
    geoCircle     : Result.Circle     := NonSymmetricMirror(Obj.Circle    ,Ratio,Line);
  end;
end;
(* End of Non-symmetric Mirror *)


function Distance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;
var
  dx : TGeoFloat;
  dy : TGeoFloat;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  Result := Sqrt(dx * dx + dy * dy);
end;
(* End of Distance *)


function Distance(const Point1,Point2:TGeoPoint2D):TGeoFloat;
begin
  Result := Distance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of Distance *)


function Distance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;
var
  dx : TGeoFloat;
  dy : TGeoFloat;
  dz : TGeoFloat;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  dz := z2 - z1;
  Result := Sqrt(dx * dx + dy * dy + dz * dz);
end;
(* End of Distance *)


function Distance(const Point1,Point2:TGeoPoint3D):TGeoFloat;
begin
  Result := Distance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of Distance *)


function Distance(const Point:TGeoPoint2D; const Segment:TGeoSegment2D):TGeoFloat;
begin
  Result := MinimumDistanceFromPointToSegment(Point,Segment);
end;
(* End of Distance *)


function Distance(const Point:TGeoPoint2D; const Rectangle:TGeoRectangle):TGeoFloat;
begin
  Result := MinimumDistanceFromPointToRectangle(Point,Rectangle);
end;
(* End of Distance *)


function Distance(const Point:TGeoPoint2D; const Triangle:TGeoTriangle2D):TGeoFloat;
begin
  Result := MinimumDistanceFromPointToTriangle(Point,Triangle);
end;
(* End of Distance *)


function Distance(const Point:TGeoPoint2D; const Quadix:TQuadix2D):TGeoFloat;
begin
  Result := sqrt(LayDistance(Point,Quadix));
end;
(* End of Distance *)


function Distance(const Line1,Line2:TGeoLine2D):TGeoFloat;
begin
  Result := DistanceLineToLine(
                               Line1[1].x,Line1[1].y,Line1[2].x,Line1[2].y,
                               Line2[1].x,Line2[1].y,Line2[2].x,Line2[2].y
                              );
end;
(* End of Distance *)


function Distance(const Line1,Line2:TGeoLine3D):TGeoFloat;
begin
  Result := DistanceLineToLine(
                               Line1[1].x,Line1[1].y,Line1[1].z,Line1[2].x,Line1[2].y,Line1[2].z,
                               Line2[1].x,Line2[1].y,Line2[1].z,Line2[2].x,Line2[2].y,Line2[2].z
                              );
end;
(* End of Distance *)


function Distance(const Segment1,Segment2:TGeoSegment2D):TGeoFloat;
begin
  Result := DistanceSegmentToSegment(
                                     Segment1[1].x,Segment1[1].y,Segment1[2].x,Segment1[2].y,
                                     Segment2[1].x,Segment2[1].y,Segment2[2].x,Segment2[2].y
                                    );
end;
(* End of Distance *)


function Distance(const Segment1,Segment2:TGeoSegment3D):TGeoFloat;
begin
  Result := DistanceSegmentToSegment(
                                     Segment1[1].x,Segment1[1].y,Segment1[1].z,Segment1[2].x,Segment1[2].y,Segment1[2].z,
                                     Segment2[1].x,Segment2[1].y,Segment2[1].z,Segment2[2].x,Segment2[2].y,Segment2[2].z
                                    );
end;
(* End of Distance *)


function Distance(const Segment:TGeoSegment2D):TGeoFloat;
begin
  Result := Distance(Segment[1],Segment[2]);
end;
(* End of Distance *)


function Distance(const Segment:TGeoSegment3D):TGeoFloat;
begin
  Result := Distance(Segment[1],Segment[2]);
end;
(* End of Distance *)


function Distance(const Segment:TGeoSegment2D; const Triangle:TGeoTriangle2D):TGeoFloat;
begin
  Result := sqrt(LayDistance(Segment,Triangle));
end;
(* End of Distance *)


function Distance(const Segment:TGeoSegment3D; const Triangle:TGeoTriangle3D):TGeoFloat;
begin
  Result := sqrt(LayDistance(Segment,Triangle));
end;
(* End of Distance *)


function Distance(const Triangle1,Triangle2:TGeoTriangle2D):TGeoFloat;
var
  i : Integer;
begin
  Result := Min(
                MinimumDistanceFromPointToTriangle(Triangle1[1],Triangle2),
                MinimumDistanceFromPointToTriangle(Triangle2[1],Triangle1)
               );

  for i := 2 to 3 do
  begin
    if IsEqual(Result,Zero) then Exit;
    Result := Min(
                  Result,
                  Min(
                      MinimumDistanceFromPointToTriangle(Triangle1[i],Triangle2),
                      MinimumDistanceFromPointToTriangle(Triangle2[i],Triangle1)
                     )
                 );
  end
end;
(* End of Distance *)


function Distance(const Triangle:TGeoTriangle2D; const Rectangle:TGeoRectangle):TGeoFloat;
begin
  if Intersect(Triangle,Rectangle) then
    Result := Zero
  else
    Result := Min(
                  Min(
                      Distance(RectangleEdge(Rectangle,1),Triangle),
                      Distance(RectangleEdge(Rectangle,2),Triangle)
                     ),
                  Min(
                      Distance(RectangleEdge(Rectangle,3),Triangle),
                      Distance(RectangleEdge(Rectangle,4),Triangle)
                     )
                 );
end;
(* End of Distance *)


function Distance(const Rectangle1,Rectangle2:TGeoRectangle):TGeoFloat;
var
  Rec1 : TGeoRectangle;
  Rec2 : TGeoRectangle;
begin
  if Intersect(Rectangle1,Rectangle2) then
    Result := Zero
  else
  begin
    Rec1 := AABB(Rectangle1);
    Rec2 := AABB(Rectangle2);

    if Rec1[2].y < Rec2[1].y then
      Result := Distance(EquateSegment(Rec1[1].x,Rec1[2].y,Rec1[2].x,Rec1[2].y),EquateSegment(Rec2[1].x,Rec2[1].y,Rec2[2].x,Rec2[1].y))
    else if Rec1[1].y > Rec2[2].y then
      Result := Distance(EquateSegment(Rec1[1].x,Rec1[1].y,Rec1[2].x,Rec1[1].y),EquateSegment(Rec2[1].x,Rec2[2].y,Rec2[2].x,Rec2[2].y))
    else if Rec1[2].x < Rec2[1].x then
      Result := Distance(EquateSegment(Rec1[2].x,Rec1[1].y,Rec1[2].x,Rec1[2].y),EquateSegment(Rec2[1].x,Rec2[1].y,Rec2[1].x,Rec2[2].y))
    else if Rec1[1].x > Rec2[2].x then
      Result := Distance(EquateSegment(Rec1[1].x,Rec1[1].y,Rec1[1].x,Rec1[2].y),EquateSegment(Rec2[2].x,Rec2[1].y,Rec2[2].x,Rec2[2].y))
    else
      Result := Zero;
  end;
end;
(* End of Distance *)


function Distance(const Segment:TGeoSegment2D; const Rectangle:TGeoRectangle):TGeoFloat;
begin
  Result := Min(
                Min(Distance(Segment,RectangleEdge(Rectangle,1)),Distance(Segment,RectangleEdge(Rectangle,2))),
                Min(Distance(Segment,RectangleEdge(Rectangle,3)),Distance(Segment,RectangleEdge(Rectangle,4)))
               );
end;
(* End of Distance *)


function Distance(const Segment:TGeoSegment2D; const Circle:TGeoCircle):TGeoFloat;
begin
   Result := Distance(ClosestPointOnCircleFromSegment(Circle,Segment),Segment);
end;
(* End of Distance *)


function Distance(const Triangle : TGeoTriangle2D; const Circle:TGeoCircle):TGeoFloat;
var
  Point1 : TGeoPoint2D;
  Point2 : TGeoPoint2D;
begin
  if Intersect(Triangle,Circle) then
    Result := Zero
  else
  begin
    Point1 := ClosestPointOnTriangleFromPoint(Triangle,Circle.x,Circle.y);
    Point2 := ClosestPointOnCircleFromPoint(Circle,Point1);
    Result := Distance(Point1,Point2);
  end;
end;
(* End of Distance *)


function Distance(const Rectangle : TGeoRectangle; const Circle:TGeoCircle):TGeoFloat;
var
  Point1 : TGeoPoint2D;
  Point2 : TGeoPoint2D;
begin
  if Intersect(Rectangle,Circle) then
    Result := Zero
  else
  begin
    Point1 := ClosestPointOnRectangleFromPoint(Rectangle,Circle.x,Circle.y);
    Point2 := ClosestPointOnCircleFromPoint(Circle,Point1);
    Result := Distance(Point1,Point2);
  end;
end;
(* End of Distance *)


function Distance(const Point : TGeoPoint2D; const Circle:TGeoCircle):TGeoFloat;
begin
  if PointIncircle(Point,Circle) then
    Result := Zero
  else
    Result := Distance(Point,ClosestPointOnCircleFromPoint(Circle,Point));
end;
(* End of Distance *)


function Distance(const Circle1,Circle2:TGeoCircle):TGeoFloat;
var
  Dist : TGeoFloat;
begin
  Dist := Distance(Circle1.x,Circle1.y,Circle2.x,Circle2.y);
  if Dist > Circle1.Radius + Circle2.Radius then
    Result := Dist - (Circle1.Radius + Circle2.Radius)
  else
    Result := Zero;
end;
(* End of Distance *)


function Distance(const Sphere1,Sphere2:TGeoSphere):TGeoFloat;
var
  Dist : TGeoFloat;
begin
  Dist := Distance(Sphere1.x,Sphere1.y,Sphere1.z,Sphere2.x,Sphere2.y,Sphere2.z);
  if Dist > Sphere1.Radius + Sphere2.Radius then
    Result := Dist - (Sphere1.Radius + Sphere2.Radius)
  else
    Result := Zero;
end;
(* End of Distance *)


function Distance(const Obj1,Obj2:TGeometricObject):TGeoFloat;
begin
  Result := Zero;
  if (Obj1.ObjectType = geoPoint2D) and (Obj2.ObjectType = geoPoint2D) then
    Result := Distance(Obj1.Point2D,Obj2.Point2D)
  else if (Obj1.ObjectType = geoPoint3D) and (Obj2.ObjectType = geoPoint3D) then
    Result := Distance(Obj1.Point3D,Obj2.Point3D)
  else if (Obj1.ObjectType = geoCircle) and (Obj2.ObjectType = geoCircle) then
    Result := Distance(Obj1.Circle,Obj2.Circle)
  else if (Obj1.ObjectType = geoSegment3D) and (Obj2.ObjectType = geoSegment3D) then
    Result := Distance(Obj1.Segment3D,Obj2.Segment3D);
end;
(* End of Distance *)


function LayDistance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;
var
  dx : TGeoFloat;
  dy : TGeoFloat;
begin
  dx := (x2 - x1);
  dy := (y2 - y1);
  Result := dx * dx + dy * dy;
end;
(* End of LayDistance *)


function LayDistance(const Point1,Point2:TGeoPoint2D):TGeoFloat;
begin
  Result := LayDistance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of LayDistance *)


function LayDistance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;
var
  dx : TGeoFloat;
  dy : TGeoFloat;
  dz : TGeoFloat;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  dz := z2 - z1;
  Result := dx * dx + dy * dy + dz * dz;
end;
(* End of LayDistance *)


function LayDistance(const Point1,Point2:TGeoPoint3D):TGeoFloat;
begin
  Result := LayDistance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of LayDistance *)


function LayDistance(const Point:TGeoPoint2D; const Triangle:TGeoTriangle2D):TGeoFloat;
begin
  Result := LayDistance(Point,ClosestPointOnTriangleFromPoint(Triangle,Point));
end;
(* End of LayDistance *)


function LayDistance(const Point:TGeoPoint2D; const Quadix:TQuadix2D):TGeoFloat;
begin
  Result := LayDistance(Point,ClosestPointOnQuadixFromPoint(Quadix,Point));
end;
(* End of LayDistance *)


function LayDistance(const Segment1,Segment2:TGeoSegment2D):TGeoFloat;
begin
  Result := LayDistanceSegmentToSegment(
                                        Segment1[1].x,Segment1[1].y,Segment1[2].x,Segment1[2].y,
                                        Segment2[1].x,Segment2[1].y,Segment2[2].x,Segment2[2].y
                                        );
end;
(* End of LayDistance *)


function LayDistance(const Segment1,Segment2:TGeoSegment3D):TGeoFloat;
begin
  Result := LayDistanceSegmentToSegment(
                                        Segment1[1].x,Segment1[1].y,Segment1[1].z,Segment1[2].x,Segment1[2].y,Segment1[2].z,
                                        Segment2[1].x,Segment2[1].y,Segment2[1].z,Segment2[2].x,Segment2[2].y,Segment2[2].z
                                        );
end;
(* End of LayDistance *)


function LayDistance(const Line1,Line2:TGeoLine2D):TGeoFloat;
begin
  Result := LayDistanceLineToLine(
                                  Line1[1].x,Line1[1].y,Line1[2].x,Line1[2].y,
                                  Line2[1].x,Line2[1].y,Line2[2].x,Line2[2].y
                                 );
end;
(* End of Distance *)


function LayDistance(const Line1,Line2:TGeoLine3D):TGeoFloat;
begin
  Result := LayDistanceLineToLine(
                                  Line1[1].x,Line1[1].y,Line1[1].z,Line1[2].x,Line1[2].y,Line1[2].z,
                                  Line2[1].x,Line2[1].y,Line2[1].z,Line2[2].x,Line2[2].y,Line2[2].z
                                 );
end;
(* End of Distance *)


function LayDistance(const Segment:TGeoSegment2D):TGeoFloat;
begin
  Result := LayDistance(Segment[1],Segment[2]);
end;
(* End of LayDistance *)


function LayDistance(const Segment:TGeoSegment3D):TGeoFloat;
begin
  Result := LayDistance(Segment[1],Segment[2]);
end;
(* End of LayDistance *)


function LayDistance(const Segment:TGeoSegment2D; const Triangle:TGeoTriangle2D):TGeoFloat;
begin
  Result := Min(
                Min(
                    LayDistanceSegmentToSegment(
                                                Segment[1].x, Segment[1].y, Segment[2].x, Segment[2].y,
                                                Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y
                                               ),
                    LayDistanceSegmentToSegment(
                                                Segment[1].x, Segment[1].y, Segment[2].x, Segment[2].y,
                                                Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y
                                               )
                   ),
                LayDistanceSegmentToSegment(
                                            Segment[1].x, Segment[1].y, Segment[2].x, Segment[2].y,
                                            Triangle[3].x,Triangle[3].y,Triangle[1].x,Triangle[1].y
                                           )
            );
end;
(* End of LayDistance *)


function LayDistance(const Segment:TGeoSegment3D; const Triangle:TGeoTriangle3D):TGeoFloat;
begin
  Result := Min(
                Min(
                    LayDistanceSegmentToSegment(
                                                Segment[1].x, Segment[1].y, Segment[1].z, Segment[2].x, Segment[2].y, Segment[2].z,
                                                Triangle[1].x,Triangle[1].y,Triangle[1].z,Triangle[2].x,Triangle[2].y,Triangle[2].z
                                               ),
                    LayDistanceSegmentToSegment(
                                                Segment[1].x, Segment[1].y, Segment[1].z, Segment[2].x, Segment[2].y, Segment[2].z,
                                                Triangle[2].x,Triangle[2].y,Triangle[2].z,Triangle[3].x,Triangle[3].y,Triangle[3].z
                                               )
                   ),
                LayDistanceSegmentToSegment(
                                            Segment[1].x, Segment[1].y, Segment[2].x, Segment[2].y,
                                            Triangle[3].x,Triangle[3].y,Triangle[1].x,Triangle[1].y
                                           )
            );
end;


function ManhattanDistance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;
begin
  Result := Abs(x2 - x1) + Abs(y2 - y1);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const Point1,Point2:TGeoPoint2D):TGeoFloat;
begin
  Result := ManhattanDistance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;
begin
  Result := Abs(x2 - x1) + Abs(y2 - y1) + Abs(z2 - z1);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const Point1,Point2:TGeoPoint3D):TGeoFloat;
begin
  Result := ManhattanDistance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const Segment:TGeoSegment2D):TGeoFloat;
begin
  Result := ManhattanDistance(Segment[1],Segment[2]);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const Segment:TGeoSegment3D):TGeoFloat;
begin
  Result := ManhattanDistance(Segment[1],Segment[2]);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const Circle1,Circle2:TGeoCircle):TGeoFloat;
begin
  Result := ManhattanDistance(Circle1.x,Circle1.y,Circle2.x,Circle2.y);
end;
(* End of ManhattanDistance *)


function VectorSumDistance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;
begin
  Result := Abs(x2 - x1) + Abs(y2 - y1);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Point1,Point2:TGeoPoint2D):TGeoFloat;
begin
  Result := VectorSumDistance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;
begin
  Result := Abs(x2 - x1) + Abs(y2 - y1) + Abs(z2 - z1);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Point1,Point2:TGeoPoint3D):TGeoFloat;
begin
  Result := VectorSumDistance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Segment:TGeoSegment2D):TGeoFloat;
begin
  Result := VectorSumDistance(Segment[1],Segment[2]);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Segment:TGeoSegment3D):TGeoFloat;
begin
  Result := VectorSumDistance(Segment[1],Segment[2]);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Circle1,Circle2:TGeoCircle):TGeoFloat;
begin
  Result := VectorSumDistance(Circle1.x,Circle1.y,Circle2.x,Circle2.y);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Obj1,Obj2:TGeometricObject):TGeoFloat;
begin
  Result := Zero;
  if (Obj1.ObjectType = geoPoint2D) and (Obj2.ObjectType = geoPoint2D) then
    Result := Distance(Obj1.Point2D,Obj2.Point2D)
  else if (Obj1.ObjectType = geoPoint3D) and (Obj2.ObjectType = geoPoint3D) then
    Result := Distance(Obj1.Point3D,Obj2.Point3D)
  else if (Obj1.ObjectType = geoCircle) and (Obj2.ObjectType = geoCircle) then
    Result := Distance(Obj1.Circle,Obj2.Circle)
end;
(* End of VectorSumDistance *)


function ChebyshevDistance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;
begin
  Result := Max(abs(x1-x2),abs(y1-y2));
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const Point1,Point2:TGeoPoint2D):TGeoFloat;
begin
  Result := ChebyshevDistance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;
begin
  Result := Max(Max(abs(x1 - x2),abs(y1 - y2)),abs(z1 - z2));
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const Point1,Point2:TGeoPoint3D):TGeoFloat;
begin
  Result := ChebyshevDistance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const Segment:TGeoSegment2D):TGeoFloat;
begin
  Result := ChebyshevDistance(Segment[1],Segment[2]);
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const Segment:TGeoSegment3D):TGeoFloat;
begin
  Result := ChebyshevDistance(Segment[1],Segment[2]);
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const Circle1,Circle2:TGeoCircle):TGeoFloat;
begin
  Result := Max(abs(Circle1.x - Circle2.x),abs(Circle1.y - Circle2.y));
end;
(* End of ChebyshevDistance *)


function InverseChebyshevDistance(const x1,y1,x2,y2:TGeoFloat):TGeoFloat;
begin
  Result := Min(abs(x1 - x2),abs(y1 - y2));
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const Point1,Point2:TGeoPoint2D):TGeoFloat;
begin
  Result := InverseChebyshevDistance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;
begin
  Result := Min(Min(abs(x1 - x2),abs(y1 - y2)),abs(z1 - z2));
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const Point1,Point2:TGeoPoint3D):TGeoFloat;
begin
  Result := InverseChebyshevDistance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const Segment:TGeoSegment2D):TGeoFloat;
begin
  Result := InverseChebyshevDistance(Segment[1],Segment[2]);
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const Segment:TGeoSegment3D):TGeoFloat;
begin
  Result := InverseChebyshevDistance(Segment[1],Segment[2]);
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const Circle1,Circle2:TGeoCircle):TGeoFloat;
begin
  Result := Min(abs(Circle1.x - Circle2.x),abs(Circle1.y - Circle2.y));
end;
(* End of InverseChebyshevDistance *)


function DistanceSegmentToSegment(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):TGeoFloat;
begin
  Result := sqrt(LayDistanceSegmentToSegment(x1,y1,x2,y2,x3,y3,x4,y4));
end;
(* End of DistanceSegmentToSegment *)


function DistanceSegmentToSegment(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat):TGeoFloat;
begin
  Result := sqrt(LayDistanceSegmentToSegment(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4));
end;
(* End of DistanceSegmentToSegment *)


function LayDistanceSegmentToSegment(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):TGeoFloat;
var
  ux : TGeoFloat;
  uy : TGeoFloat;
  vx : TGeoFloat;
  vy : TGeoFloat;
  wx : TGeoFloat;
  wy : TGeoFloat;
  a  : TGeoFloat;
  b  : TGeoFloat;
  c  : TGeoFloat;
  d  : TGeoFloat;
  e  : TGeoFloat;
  Dt : TGeoFloat;
  sc : TGeoFloat;
  sN : TGeoFloat;
  sD : TGeoFloat;
  tc : TGeoFloat;
  tN : TGeoFloat;
  tD : TGeoFloat;
  dx : TGeoFloat;
  dy : TGeoFloat;
begin
  ux := x2 - x1;
  uy := y2 - y1;

  vx := x4 - x3;
  vy := y4 - y3;

  wx := x1 - x3;
  wy := y1 - y3;

  a  := (ux * ux + uy * uy);
  b  := (ux * vx + uy * vy);
  c  := (vx * vx + vy * vy);
  d  := (ux * wx + uy * wy);
  e  := (vx * wx + vy * wy);
  Dt := a * c - b * b;

  sD := Dt;
  tD := Dt;

  if IsEqual(Dt,Zero) then
  begin
    sN := Zero;
    sD := 1.0;
    tN := e;
    tD := c;
  end
  else
  begin
    sN := (b * e - c * d);
    tN := (a * e - b * d);
    if sN < Zero then
    begin
      sN := Zero;
      tN := e;
      tD := c;
    end
    else if sN > sD then
    begin
      sN := sD;
      tN := e + b;
      tD := c;
    end;
  end;

  if tN < Zero then
  begin
    tN := Zero;
    if -d < Zero then
      sN := Zero
    else if -d > a then
      sN := sD
    else
    begin
      sN := -d;
      sD := a;
    end;
  end
  else if tN > tD  then
  begin
    tN := tD;
    if (-d + b) < Zero then
      sN := Zero
    else if (-d + b) > a then
      sN := sD
    else
    begin
      sN := (-d + b);
      sD := a;
    end;
  end;

  if IsEqual(sN,Zero) then
    sc := Zero
  else
    sc := sN / sD;

  if IsEqual(tN,Zero) then
    tc := Zero
  else
    tc := tN / tD;

  dx := wx + (sc * ux) - (tc * vx);
  dy := wy + (sc * uy) - (tc * vy);
  Result := dx * dx + dy * dy;
end;
(* End of LayDistanceSegmentToSegment *)


function LayDistanceSegmentToSegment(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat):TGeoFloat;
var
  ux : TGeoFloat;
  uy : TGeoFloat;
  uz : TGeoFloat;
  vx : TGeoFloat;
  vy : TGeoFloat;
  vz : TGeoFloat;
  wx : TGeoFloat;
  wy : TGeoFloat;
  wz : TGeoFloat;
  a  : TGeoFloat;
  b  : TGeoFloat;
  c  : TGeoFloat;
  d  : TGeoFloat;
  e  : TGeoFloat;
  Dt : TGeoFloat;
  sc : TGeoFloat;
  sN : TGeoFloat;
  sD : TGeoFloat;
  tc : TGeoFloat;
  tN : TGeoFloat;
  tD : TGeoFloat;
  dx : TGeoFloat;
  dy : TGeoFloat;
  dz : TGeoFloat;
begin
  ux := x2 - x1;
  uy := y2 - y1;
  uz := z2 - z1;

  vx := x4 - x3;
  vy := y4 - y3;
  vz := z4 - z3;

  wx := x1 - x3;
  wy := y1 - y3;
  wz := z1 - z3;

  a  := (ux * ux + uy * uy + uz * uz);
  b  := (ux * vx + uy * vy + uz * vz);
  c  := (vx * vx + vy * vy + vz * vz);
  d  := (ux * wx + uy * wy + uz * wz);
  e  := (vx * wx + vy * wy + vz * wz);
  Dt := a * c - b * b;

  sD := Dt;
  tD := Dt;

  if IsEqual(Dt,Zero) then
  begin
    sN := Zero;
    sD := 1.0;
    tN := e;
    tD := c;
  end
  else
  begin
    sN := (b * e - c * d);
    tN := (a * e - b * d);
    if sN < Zero then
    begin
      sN := Zero;
      tN := e;
      tD := c;
    end
    else if sN > sD then
    begin
      sN := sD;
      tN := e + b;
      tD := c;
    end;
  end;

  if tN < Zero then
  begin
    tN := Zero;
    if -d < Zero then
      sN := Zero
    else if -d > a then
      sN := sD
    else
    begin
      sN := -d;
      sD := a;
    end;
  end
  else if tN > tD  then
  begin
    tN := tD;
    if (-d + b) < Zero then
      sN := Zero
    else if (-d + b) > a then
      sN := sD
    else
    begin
      sN := (-d + b);
      sD := a;
    end;
  end;

  if IsEqual(sN,Zero) then
    sc := Zero
  else
    sc := sN / sD;

  if IsEqual(tN,Zero) then
    tc := Zero
  else
    tc := tN / tD;

  dx := wx + (sc * ux) - (tc * vx);
  dy := wy + (sc * uy) - (tc * vy);
  dz := wz + (sc * uz) - (tc * vz);
  Result := dx * dx + dy * dy + dz * dz;
end;
(* End of LayDistanceSegmentToSegment *)


function DistanceLineToLine(const x1,y1,x2,y2,x3,y3,x4,y4: TGeoFloat):TGeoFloat;
begin
  Result := sqrt(LayDistanceLineToLine(x1,y1,x2,y2,x3,y3,x4,y4));
end;
(* End of DistanceLineToLine *)


function DistanceLineToLine(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4: TGeoFloat):TGeoFloat;
begin
  Result := sqrt(LayDistanceLineToLine(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4));
end;
(* End of DistanceLineToLine *)


function LayDistanceLineToLine(const x1,y1,x2,y2,x3,y3,x4,y4: TGeoFloat):TGeoFloat;
var
  ux : TGeoFloat;
  uy : TGeoFloat;
  vx : TGeoFloat;
  vy : TGeoFloat;
  wx : TGeoFloat;
  wy : TGeoFloat;
  a  : TGeoFloat;
  b  : TGeoFloat;
  c  : TGeoFloat;
  d  : TGeoFloat;
  e  : TGeoFloat;
  Dt : TGeoFloat;
  sc : TGeoFloat;
  tc : TGeoFloat;
  dx : TGeoFloat;
  dy : TGeoFloat;

begin
  ux := x2 - x1;
  uy := y2 - y1;

  vx := x4 - x3;
  vy := y4 - y3;

  if NotEqual(ux * vy,uy * vx) then
  begin
    Result := Zero;
    Exit;
  end;

  wx := x1 - x3;
  wy := y1 - y3;

  a  := (ux * ux + uy * uy);
  b  := (ux * vx + uy * vy);
  c  := (vx * vx + vy * vy);
  d  := (ux * wx + uy * wy);
  e  := (vx * wx + vy * wy);
  Dt := a * c - b * b;

  if IsEqual(Dt,Zero) then
  begin
    sc := Zero;
    if b > c then
      tc := d / b
    else
      tc := e /c;
  end
  else
  begin
    sc := (b * e - c * d) / Dt;
    tc := (a * e - b * d) / Dt;
  end;

  dx := wx + (sc * ux) - (tc * vx);
  dy := wy + (sc * uy) - (tc * vy);
  Result := dx * dx + dy * dy;
end;
(* End of LayDistanceLineToLine *)


function LayDistanceLineToLine(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4: TGeoFloat):TGeoFloat;
var
  ux : TGeoFloat;
  uy : TGeoFloat;
  uz : TGeoFloat;
  vx : TGeoFloat;
  vy : TGeoFloat;
  vz : TGeoFloat;
  wx : TGeoFloat;
  wy : TGeoFloat;
  wz : TGeoFloat;
  a  : TGeoFloat;
  b  : TGeoFloat;
  c  : TGeoFloat;
  d  : TGeoFloat;
  e  : TGeoFloat;
  Dt : TGeoFloat;
  sc : TGeoFloat;
  tc : TGeoFloat;
  dx : TGeoFloat;
  dy : TGeoFloat;
  dz : TGeoFloat;
begin
  ux := x2 - x1;
  uy := y2 - y1;
  uz := z2 - z1;

  vx := x4 - x3;
  vy := y4 - y3;
  vz := z4 - z3;

  wx := x1 - x3;
  wy := y1 - y3;
  wz := z1 - z3;

  a  := (ux * ux + uy * uy + uz * uz);
  b  := (ux * vx + uy * vy + uz * vz);
  c  := (vx * vx + vy * vy + vz * vz);
  d  := (ux * wx + uy * wy + uz * wz);
  e  := (vx * wx + vy * wy + vz * wz);
  Dt := a * c - b * b;

  if IsEqual(Dt,Zero) then
  begin
    sc := Zero;
    if b > c then
      tc := d / b
    else
      tc := e /c;
  end
  else
  begin
    sc := (b * e - c * d) / Dt;
    tc := (a * e - b * d) / Dt;
  end;

  dx := wx + (sc * ux) - (tc * vx);
  dy := wy + (sc * uy) - (tc * vy);
  dz := wz + (sc * uz) - (tc * vz);
  Result := dx * dx + dy * dy + dz * dz;
end;
(* End of LayDistanceLineToLine *)


function TriangleType(const x1,y1,x2,y2,x3,y3:TGeoFloat):eTriangletype;
begin
  if IsEquilateralTriangle(x1,y1,x2,y2,x3,y3)    then Result := etEquilateral
   else
    if IsIsoscelesTriangle(x1,y1,x2,y2,x3,y3)    then Result := etIsosceles
     else
      if IsRightTriangle(x1,y1,x2,y2,x3,y3)      then Result := etRight
       else
        if IsScaleneTriangle(x1,y1,x2,y2,x3,y3)  then Result := etScalene
         else
          if IsObtuseTriangle(x1,y1,x2,y2,x3,y3) then Result := etObtuse
           else
             Result := etUnknown;
end;
(* End of Triangletype *)


function TriangleType(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):eTriangletype;
begin
  if IsEquilateralTriangle(x1,y1,z1,x2,y2,z2,x3,y3,z3)    then Result := etEquilateral
   else
    if IsIsoscelesTriangle(x1,y1,z1,x2,y2,z2,x3,y3,z3)    then Result := etIsosceles
     else
      if IsRightTriangle(x1,y1,z1,x2,y2,z2,x3,y3,z3)      then Result := etRight
       else
        if IsScaleneTriangle(x1,y1,z1,x2,y2,z2,x3,y3,z3)  then Result := etScalene
         else
          if IsObtuseTriangle(x1,y1,z1,x2,y2,z2,x3,y3,z3) then Result := etObtuse
           else
             Result:= etUnknown;
end;
(* End of Triangletype *)


function TriangleType(const Point1,Point2,Point3:TGeoPoint2D):eTriangletype;
begin
  Result := TriangleType(Point1.x,Point1.y,
                         Point2.x,Point2.y,
                         Point3.x,Point3.y);
end;
(* End of Triangletype *)


function TriangleType(const Point1,Point2,Point3:TGeoPoint3D):eTriangletype;
begin
  Result := TriangleType(Point1.x,Point1.y,Point1.z,
                         Point2.x,Point2.y,Point2.z,
                         Point3.x,Point3.y,Point3.z);
end;
(* End of Triangletype *)


function TriangleType(const Triangle:TGeoTriangle2D):eTriangletype;
begin
  Result := TriangleType(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of Triangletype *)


function TriangleType(const Triangle:TGeoTriangle3D):eTriangletype;
begin
  Result := TriangleType(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of Triangletype *)


function IsEquilateralTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;
var
  d1 : TGeoFloat;
  d2 : TGeoFloat;
  d3 : TGeoFloat;
begin
  d1 := LayDistance(x1,y1,x2,y2);
  d2 := LayDistance(x2,y2,x3,y3);
  d3 := LayDistance(x3,y3,x1,y1);
  Result := (IsEqual(d1,d2) and IsEqual(d2,d3));
end;
(* End of IsEquilateralTriangle *)


function IsEquilateralTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;
var
  d1 : TGeoFloat;
  d2 : TGeoFloat;
  d3 : TGeoFloat;
begin
  d1 := LayDistance(x1,y1,z1,x2,y2,z2);
  d2 := LayDistance(x2,y2,z2,x3,y3,z3);
  d3 := LayDistance(x3,y3,z3,x1,y1,z1);
  Result := (IsEqual(d1,d2) and IsEqual(d2,d3));
end;
(* End of IsEquilateralTriangle *)


function IsEquilateralTriangle(const Point1,Point2,Point3:TGeoPoint2D):Boolean;
begin
  Result := IsEquilateralTriangle(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of IsEquilateralTriangle *)


function IsEquilateralTriangle(const Point1,Point2,Point3:TGeoPoint3D):Boolean;
begin
  Result := IsEquilateralTriangle(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z);
end;
(* End of IsEquilateralTriangle *)


function IsEquilateralTriangle(const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := IsEquilateralTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsEquilateralTriangle *)


function IsEquilateralTriangle(const Triangle:TGeoTriangle3D):Boolean;
begin
  Result := IsEquilateralTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsEquilateralTriangle *)


function IsIsoscelesTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;
var
  d1 : TGeoFloat;
  d2 : TGeoFloat;
  d3 : TGeoFloat;
begin
  d1 := LayDistance(x1,y1,x2,y2);
  d2 := LayDistance(x2,y2,x3,y3);
  d3 := LayDistance(x3,y3,x1,y1);
  Result :=((IsEqual(d1,d2) or  IsEqual(d1,d3)) and NotEqual(d2,d3)) or
            (IsEqual(d2,d3) and NotEqual(d2,d1));
end;
(* End of IsIsoscelesTriangle *)


function IsIsoscelesTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;
var
  d1 : TGeoFloat;
  d2 : TGeoFloat;
  d3 : TGeoFloat;
begin
  d1 := LayDistance(x1,y1,z1,x2,y2,z2);
  d2 := LayDistance(x2,y2,z2,x3,y3,z3);
  d3 := LayDistance(x3,y3,z3,x1,y1,z1);
  Result := (
             (IsEqual(d1,d2) or  IsEqual(d1,d3)) and NotEqual(d2,d3)) or
             (IsEqual(d2,d3) and NotEqual(d2,d1)
            );
end;
(* End of IsIsoscelesTriangle *)


function IsIsoscelesTriangle(const Point1,Point2,Point3:TGeoPoint2D):Boolean;
begin
  Result := IsIsoscelesTriangle(Point1.x, Point1.y, Point2.x, Point2.y, Point3.x, Point3.y);
end;
(* End of IsIsoscelesTriangle *)


function IsIsoscelesTriangle(const Point1,Point2,Point3:TGeoPoint3D):Boolean;
begin
  Result := IsIsoscelesTriangle(Point1.x, Point1.y,Point1.z, Point2.x, Point2.y,Point2.z, Point3.x, Point3.y, Point3.z);
end;
(* End of IsIsoscelesTriangle *)


function IsIsoscelesTriangle(const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := IsIsoscelesTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsIsoscelesTriangle *)


function IsIsoscelesTriangle(const Triangle:TGeoTriangle3D):Boolean;
begin
  Result := IsIsoscelesTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of *)


function IsRightTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;
var
  d1 : TGeoFloat;
  d2 : TGeoFloat;
  d3 : TGeoFloat;
begin
  d1 := LayDistance(x1,y1,x2,y2);
  d2 := LayDistance(x2,y2,x3,y3);
  d3 := LayDistance(x3,y3,x1,y1);

  Result := (
             IsEqual(d1 + d2,d3) or
             IsEqual(d1 + d3,d2) or
             IsEqual(d3 + d2,d1)
            );
end;
(* End of IsRightTriangle *)


function IsRightTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;
var
  d1 : TGeoFloat;
  d2 : TGeoFloat;
  d3 : TGeoFloat;
begin
  d1 := LayDistance(x1,y1,z1,x2,y2,z2);
  d2 := LayDistance(x2,y2,z2,x3,y3,z3);
  d3 := LayDistance(x3,y3,z3,x1,y1,z1);

  Result := (
             IsEqual(d1 + d2,d3) or
             IsEqual(d1 + d3,d2) or
             IsEqual(d3 + d2,d1)
            );
end;
(* End of IsRightTriangle *)


function IsRightTriangle(const Point1,Point2,Point3:TGeoPoint2D):Boolean;
begin
  Result := IsRightTriangle(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of IsRightTriangle *)


function IsRightTriangle(const Point1,Point2,Point3:TGeoPoint3D):Boolean;
begin
  Result := IsRightTriangle(Point1.x,Point1.y, Point1.z,Point2.x,Point2.y, Point2.z,Point3.x,Point3.y,Point3.z);
end;
(* End of IsRightTriangle *)


function IsRightTriangle(const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := IsRightTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsRightTriangle *)


function IsRightTriangle(const Triangle:TGeoTriangle3D):Boolean;
begin
  Result := IsRightTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsRightTriangle *)


function IsScaleneTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;
var
  d1 : TGeoFloat;
  d2 : TGeoFloat;
  d3 : TGeoFloat;
begin
  d1 := LayDistance(x1,y1,x2,y2);
  d2 := LayDistance(x2,y2,x3,y3);
  d3 := LayDistance(x3,y3,x1,y1);
  Result := NotEqual(d1,d2) and NotEqual(d2,d3) and NotEqual(d3,d1);
end;
(* End of IsScaleneTriangle *)


function IsScaleneTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;
var
  d1 : TGeoFloat;
  d2 : TGeoFloat;
  d3 : TGeoFloat;
begin
  d1 := LayDistance(x1,y1,z1,x2,y2,z2);
  d2 := LayDistance(x2,y2,z2,x3,y3,z3);
  d3 := LayDistance(x3,y3,z3,x1,y1,z1);
  Result := NotEqual(d1,d2) and NotEqual(d2,d3) and NotEqual(d3,d1);
end;
(* End of IsScaleneTriangle *)


function IsScaleneTriangle(const Point1,Point2,Point3:TGeoPoint2D):Boolean;
begin
  Result := IsScaleneTriangle(Point1.x, Point1.y, Point2.x, Point2.y, Point3.x, Point3.y);
end;
(* End of IsScaleneTriangle *)


function IsScaleneTriangle(const Point1,Point2,Point3:TGeoPoint3D):Boolean;
begin
  Result := IsScaleneTriangle(Point1.x, Point1.y, Point1.z, Point2.x, Point2.y, Point2.z, Point3.x, Point3.y, Point3.z);
end;
(* End of IsScaleneTriangle *)


function IsScaleneTriangle(const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := IsScaleneTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsScaleneTriangle *)


function IsScaleneTriangle(const Triangle:TGeoTriangle3D):Boolean;
begin
 Result := IsScaleneTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsScaleneTriangle *)


function IsObtuseTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;
var
  a1 : TGeoFloat;
  a2 : TGeoFloat;
  a3 : TGeoFloat;
begin
  a1 := VertexAngle(x1,y1,x2,y2,x3,y3);
  a2 := VertexAngle(x3,y3,x1,y1,x2,y2);
  a3 := VertexAngle(x2,y2,x3,y3,x1,y1);
  Result := (a1 > 90.0) or (a2 > 90.0) or (a3 > 90.0);
end;
(* End of IsObtuseTriangle *)


function IsObtuseTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):Boolean;
var
  a1 : TGeoFloat;
  a2 : TGeoFloat;
  a3 : TGeoFloat;
begin
  a1 := VertexAngle(x1,y1,z1,x2,y2,z2,x3,y3,z3);
  a2 := VertexAngle(x3,y3,z3,x1,y1,z1,x2,y2,z2);
  a3 := VertexAngle(x2,y2,z2,x3,y3,z3,x1,y1,z1);
  Result := (a1 > 90.0) or (a2 > 90.0) or (a3 > 90.0);
end;
(* End of IsObtuseTriangle *)


function IsObtuseTriangle(const Point1,Point2,Point3:TGeoPoint2D):Boolean;
begin
  Result := IsObtuseTriangle(Point1.x, Point1.y, Point2.x, Point2.y, Point3.x, Point3.y);
end;
(* End of IsObtuseTriangle *)


function IsObtuseTriangle(const Point1,Point2,Point3:TGeoPoint3D):Boolean;
begin
  Result := IsObtuseTriangle(Point1.x, Point1.y, Point1.z, Point2.x, Point2.y, Point2.z, Point3.x, Point3.y ,Point3.z);
end;
(* End of IsObtuseTriangle *)


function IsObtuseTriangle(const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := IsObtuseTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsObtuseTriangle *)


function IsObtuseTriangle(const Triangle:TGeoTriangle3D):Boolean;
begin
  Result := IsObtuseTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsObtuseTriangle *)


function TriangleEdge(const Triangle:TGeoTriangle2D; const Edge:Integer):TGeoSegment2D;
begin
  case Edge of
    1: Result := EquateSegment(Triangle[1],Triangle[2]);
    2: Result := EquateSegment(Triangle[2],Triangle[3]);
    3: Result := EquateSegment(Triangle[3],Triangle[1]);
  end;
end;
(* Triangle Edge *)


function TriangleEdge(const Triangle:TGeoTriangle3D; const Edge:Integer):TGeoSegment3D;
begin
  case Edge of
    1: Result := EquateSegment(Triangle[1],Triangle[2]);
    2: Result := EquateSegment(Triangle[2],Triangle[3]);
    3: Result := EquateSegment(Triangle[3],Triangle[1]);
  end;
end;
(* Triangle Edge *)


function RectangleEdge(const Rectangle:TGeoRectangle; const Edge:Integer):TGeoSegment2D;
begin
  case Edge of
    1: Result := EquateSegment(Rectangle[1].x,Rectangle[1].y,Rectangle[1].x,Rectangle[1].y);
    2: Result := EquateSegment(Rectangle[2].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
    3: Result := EquateSegment(Rectangle[2].x,Rectangle[2].y,Rectangle[1].x,Rectangle[2].y);
    4: Result := EquateSegment(Rectangle[1].x,Rectangle[2].y,Rectangle[1].x,Rectangle[1].y);
  end;
end;
(* Rectangle Edge*)


function PointInTriangle(const Px,Py,x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;
var
  Or1 : Integer;
  Or2 : Integer;
  Or3 : Integer;
begin
  // (*
  Or1 := Orientation(x1,y1,x2,y2,Px,Py);
  Or2 := Orientation(x2,y2,x3,y3,Px,Py);

  if (Or1 * Or2) = -1 then
    Result := False
  else
  begin
    Or3 := Orientation(x3,y3,x1,y1,Px,Py);
    if (Or1 = Or3) or (Or3 = 0) then
      Result := True
    else if Or1 = 0 then
      Result := (Or2 * Or3) >= 0
    else if Or2 = 0 then
      Result := (Or1 * Or3) >= 0
    else
      Result := False;
  end;
  //*)
  (*
    // Note: The following code does the same as above,
    //       but is less efficient time-wise.
    Or1 := Orientation(x1,y1,x2,y2,Px,Py);
    Or2 := Orientation(x2,y2,x3,y3,Px,Py);
    Or3 := Orientation(x3,y3,x1,y1,Px,Py);

    if (Or1 = Or2) and (Or2 = Or3) then
     Result := True
    else if Or1 = 0 then
     Result := (Or2 * Or3) >= 0
    else if Or2 = 0 then
     Result := (Or1 * Or3) >= 0
    else if Or3 = 0 then
     Result := (Or1 * Or2) >= 0
    else
     Result := False;
  //*)
end;
(* End of PointInTriangle *)


function PointInTriangle(const x,y:TGeoFloat; const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := PointInTriangle(x,y,Triangle[1].x,Triangle[1].y,
                                Triangle[2].x,Triangle[2].y,
                                Triangle[3].x,Triangle[3].y);
end;
(* End of PointInTriangle *)


function PointInTriangle(const Point:TGeoPoint2D; const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := PointInTriangle(Point.x,Point.y,Triangle[1].x,Triangle[1].y,
                                            Triangle[2].x,Triangle[2].y,
                                            Triangle[3].x,Triangle[3].y);
end;
(* End of PointInTriangle *)


function PointInCircle(const Px,Py,Cx,Cy,Radius:TGeoFloat):Boolean;
begin
  Result := (LayDistance(Px,Py,Cx,Cy) <= (Radius * Radius));
end;
(* End of PointInCircle *)


function PointInCircle(const Px,Py:TGeoFloat; const Circle:TGeoCircle):Boolean;
begin
  Result := PointInCircle(Px,Py,Circle.x,Circle.y,Circle.Radius);
end;
(* End of PointInCircle *)


function PointInCircle(const Point:TGeoPoint2D; const Circle:TGeoCircle):Boolean;
begin
  Result := PointInCircle(Point.x,Point.y,Circle);
end;
(* End of PointInCircle *)


function PointOnCircle(const Px,Py:TGeoFloat; const Circle:TGeoCircle):Boolean;
begin
  Result := IsEqual(LayDistance(Px,Py,Circle.x,Circle.y),(Circle.Radius * Circle.Radius));
end;
(* End of PointInCircle *)


function PointOnCircle(const Point:TGeoPoint2D; const Circle:TGeoCircle):Boolean;
begin
  Result := PointOnCircle(Point.x,Point.y,Circle);
end;
(* End of PointInCircle *)


function TriangleInCircle(const Triangle:TGeoTriangle2D; const Circle:TGeoCircle):Boolean;
begin
  Result := PointInCircle(Triangle[1],Circle) and
            PointInCircle(Triangle[2],Circle) and
            PointInCircle(Triangle[3],Circle);
end;
(* End of TriangleInCircle *)


function TriangleOutsideCircle(const Triangle:TGeoTriangle2D; const Circle:TGeoCircle):Boolean;
begin
  Result := (not PointInCircle(Triangle[1],Circle)) and
            (not PointInCircle(Triangle[2],Circle)) and
            (not PointInCircle(Triangle[3],Circle));
end;
(* End of TriangleOutsideCircle *)


function TriangleEncompassesCircle(const Triangle:TGeoTriangle2D; const Circle:TGeoCircle):Boolean;
begin
  Result := TriangleOutsideCircle(Triangle,Circle) and
            PointInCircle(ClosestPointOnTriangleFromPoint(Triangle,Circle.x,Circle.y),Circle);
end;
(* End of TriangleEncompassesCircle *)


function RectangleInCircle(const Rectangle:TGeoRectangle; const Circle:TGeoCircle):Boolean;
begin
  Result := PointInCircle(Rectangle[1].x,Rectangle[1].y,Circle) and
            PointInCircle(Rectangle[2].x,Rectangle[2].y,Circle) and
            PointInCircle(Rectangle[1].x,Rectangle[2].y,Circle) and
            PointInCircle(Rectangle[2].x,Rectangle[1].y,Circle);
end;
(* End of RectangleInCircle *)


function RectangleOutsideCircle(const Rectangle:TGeoRectangle; const Circle:TGeoCircle):Boolean;
begin
  Result := (not PointInCircle(Rectangle[1].x,Rectangle[1].y,Circle)) and
            (not PointInCircle(Rectangle[2].x,Rectangle[2].y,Circle)) and
            (not PointInCircle(Rectangle[1].x,Rectangle[2].y,Circle)) and
            (not PointInCircle(Rectangle[2].x,Rectangle[1].y,Circle));
end;
(* End of RectangleInCircle *)


function QuadixInCircle(const Quadix:TQuadix2D; const Circle:TGeoCircle):Boolean;
begin
  Result := PointInCircle(Quadix[1],Circle) and
            PointInCircle(Quadix[2],Circle) and
            PointInCircle(Quadix[3],Circle) and
            PointInCircle(Quadix[4],Circle);
end;
(* End of QuadixInCircle *)


function QuadixOutsideCircle(const Quadix:TQuadix2D; const Circle:TGeoCircle):Boolean;
begin
  Result := (not PointInCircle(Quadix[1],Circle)) and
            (not PointInCircle(Quadix[2],Circle)) and
            (not PointInCircle(Quadix[3],Circle)) and
            (not PointInCircle(Quadix[4],Circle));
end;
(* End of QuadixInCircle *)


function PointInThreePoinTGeoCircle(const Px,Py,x1,y1,x2,y2,x3,y3:TGeoFloat):Boolean;
var
  a11 : TGeoFloat;
  a12 : TGeoFloat;
  a21 : TGeoFloat;
  a22 : TGeoFloat;
  Dx1 : TGeoFloat;
  Dx2 : TGeoFloat;
  Dx3 : TGeoFloat;
  Dy3 : TGeoFloat;
  Dy1 : TGeoFloat;
  Dy2 : TGeoFloat;
begin
  Dx1 := x1 - Px;
  Dx2 := x2 - Px;
  Dx3 := x3 - Px;
  Dy1 := y2 - Py;
  Dy2 := y3 - Py;
  Dy3 := y1 - Py;

  a11 := Dx3 * Dy1 - Dx2 * Dy2;
  a12 := Dx3 * Dy3 - Dx1 * Dy2;
  a21 := Dx2 * (x2 - x3) + Dy1 * (y2 - y3);
  a22 := Dx1 * (x1 - x3) + Dy3 * (y1 - y3);

  Result := ((a11 * a22 - a21 * a12) <= Zero);
end;
(* End of Point In Three Point Circle *)


function PointInThreePoinTGeoCircle(const Point,Point1,Point2,Point3:TGeoPoint2D):Boolean;
begin
  Result := PointInThreePoinTGeoCircle(Point.x, Point.y,
                                    Point1.x,Point1.y,
                                    Point2.x,Point2.y,
                                    Point3.x,Point3.y);
end;
(* End of Point In Three Point Circle *)


function PointInThreePoinTGeoCircle(const Point:TGeoPoint2D; const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := PointInThreePoinTGeoCircle(Point,Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of Point In Three Point Circle *)


function PointInRectangle(const Px,Py:TGeoFloat; const x1,y1,x2,y2:TGeoFloat):Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or
            ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;
(* End of PointInRectangle *)


function PointInRectangle(const Point:TGeoPoint2D; const x1,y1,x2,y2:TGeoFloat):Boolean;
begin
  Result := PointInRectangle(Point.x,Point.y,x1,y1,x2,y2);
end;
(* End of PointInRectangle *)


function PointInRectangle(const Px,Py:TGeoFloat; const Rectangle:TGeoRectangle):Boolean;
begin
  Result := PointInRectangle(Px,Py,Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
end;
(* End of PointInRectangle *)


function PointInRectangle(const Point:TGeoPoint2D; const Rectangle:TGeoRectangle):Boolean;
begin
  Result := PointInRectangle(Point.x,Point.y,Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
end;
(* End of PointInRectangle *)

function PointInBox(const Px,Py,Pz:TGeoFloat; const x1,y1,z1,x2,y2,z2:TGeoFloat):Boolean;
begin
  Result := (((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or
             ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1))) and
            (((z1 <= Pz) and (Pz <= z2)) or ((z2 <= Pz) and (Pz <= z1)));
end;
(* End Of PointInBox *)


function PointInBox(const Point:TGeoPoint3D;  const x1,y1,z1,x2,y2,z2:TGeoFloat):Boolean;
begin
 Result := PointInBox(Point.x,Point.y,Point.z,x1,y1,z1,x2,y2,z2);
end;
(* End Of PointInBox *)


function PointInBox(const Px,Py,Pz:TGeoFloat; const Box:TGeoBox):Boolean;
begin
  Result := PointInBox(Px,Py,Pz,Box[1].x,Box[1].y,Box[1].z,Box[2].x,Box[2].y,Box[2].z);
end;
(* End Of PointInBox *)


function PointInBox(const Point:TGeoPoint3D;  const Box:TGeoBox):Boolean;
begin
  Result := PointInBox(Point.x,Point.y,Point.z,Box);
end;
(* End Of PointInBox *)


function TriangleInRectangle(const Triangle:TGeoTriangle2D; const Rectangle:TGeoRectangle):Boolean;
begin
  Result := PointInRectangle(Triangle[1],Rectangle) and
            PointInRectangle(Triangle[2],Rectangle) and
            PointInRectangle(Triangle[3],Rectangle);
end;
(* End of TriangleInRectangle *)

function TriangleOutsideRectangle(const Triangle:TGeoTriangle2D; const Rectangle:TGeoRectangle):Boolean;
begin
 Result := (not PointInRectangle(Triangle[1],Rectangle)) and
           (not PointInRectangle(Triangle[2],Rectangle)) and
           (not PointInRectangle(Triangle[3],Rectangle));
end;
(* End of TriangleInRectangle *)


function QuadixInRectangle(const Quadix:TQuadix2D; const Rectangle:TGeoRectangle):Boolean;
begin
 Result := PointInRectangle(Quadix[1],Rectangle) and
           PointInRectangle(Quadix[2],Rectangle) and
           PointInRectangle(Quadix[3],Rectangle) and
           PointInRectangle(Quadix[4],Rectangle);
end;
(* End of QuadixInRectangle *)


function QuadixOutsideRectangle(const Quadix:TQuadix2D; const Rectangle:TGeoRectangle):Boolean;
begin
 Result := (not PointInRectangle(Quadix[1],Rectangle)) and
           (not PointInRectangle(Quadix[2],Rectangle)) and
           (not PointInRectangle(Quadix[3],Rectangle)) and
           (not PointInRectangle(Quadix[4],Rectangle));
end;
(* End of QuadixOutsideRectangle *)


function PointInQuadix(const Px,Py,x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;
var
  Or1 : Integer;
  Or2 : Integer;
  Or3 : Integer;
  Or4 : Integer;
begin
  Or1 := Orientation(x1,y1,x2,y2,Px,Py);
  Or2 := Orientation(x2,y2,x3,y3,Px,Py);
  Or3 := Orientation(x3,y3,x4,y4,Px,Py);
  Or4 := Orientation(x4,y4,x1,y1,Px,Py);

  if (Or1 = Or2) and (Or2 = Or3) and (Or3 = Or4) then
    Result := True
  else if Or1 = 0 then
    Result := (Or2 * Or4) = 0
  else if Or2 = 0 then
    Result := (Or1 * Or3) = 0
  else if Or3 = 0 then
    Result := (Or2 * Or4) = 0
  else if Or4 = 0 then
    Result := (Or1 * Or3) = 0
  else
    Result := False;
end;
(* End of PointInQuadix *)


function PointInQuadix(const Point,Point1,Point2,Point3,Point4:TGeoPoint2D):Boolean;
begin
  Result := PointInQuadix(Point.x,Point.y,Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y);
end;
(* End of PointInQuadix *)


function PointInQuadix(const x,y:TGeoFloat; const Quadix:TQuadix2D):Boolean;
begin
  Result := PointInQuadix(x,y,Quadix[1].x,Quadix[1].y,Quadix[2].x,Quadix[2].y,Quadix[3].x,Quadix[3].y,Quadix[4].x,Quadix[4].y);
end;
(* End of PointInQuadix *)


function PointInQuadix(const Point:TGeoPoint2D; const Quadix:TQuadix2D):Boolean;
begin
  Result := PointInQuadix(Point,Quadix[1],Quadix[2],Quadix[3],Quadix[4]);
end;
(* End of PointInQuadix *)


function TriangleInQuadix(const Triangle:TGeoTriangle2D; const Quadix:TQuadix2D):Boolean;
begin
  Result := PointInQuadix(Triangle[1],Quadix) and
            PointInQuadix(Triangle[2],Quadix) and
            PointInQuadix(Triangle[3],Quadix);
end;
(* End of TriangleInQuadix *)


function TriangleOutsideQuadix(const Triangle:TGeoTriangle2D; const Quadix:TQuadix2D):Boolean;
begin
  Result := (not PointInQuadix(Triangle[1],Quadix)) and
            (not PointInQuadix(Triangle[2],Quadix)) and
            (not PointInQuadix(Triangle[3],Quadix));
end;
(* End of TriangleInQuadix *)


function PointInSphere(const x,y,z:TGeoFloat; const Sphere:TGeoSphere):Boolean;
begin
  Result := (LayDistance(x,y,z,Sphere.z,Sphere.y,Sphere.z) <= (Sphere.Radius * Sphere.Radius));
end;
(* End of PointInSphere *)


function PointInSphere(const Point3D:TGeoPoint3D; const Sphere:TGeoSphere):Boolean;
begin
  Result := PointInSphere(Point3D.x,Point3D.y,Point3D.z,Sphere);
end;
(* End of PointInSphere *)


function PointOnSphere(const Point3D:TGeoPoint3D; const Sphere:TGeoSphere):Boolean;
begin
  Result := IsEqual(LayDistance(Point3D.x,Point3D.y,Point3D.z,Sphere.z,Sphere.y,Sphere.z),(Sphere.Radius * Sphere.Radius));
end;
(* End of PointOnSphere *)


function PolyhedronInSphere(const Polygon:TGeoPolyhedron; const Sphere:TGeoSphere):TInclusion;
var
  i         : Integer;
  j         : Integer;
  Count     : Integer;
  RealCount : Integer;
begin
  RealCount := 0;
  Count     := 0;
  for i := 0 to Length(Polygon) - 1 do
  begin
    Inc(RealCount,Length(Polygon[i]));
    for j := 0 to Length(Polygon[i]) - 1 do
    if PointInSphere(Polygon[i][j],Sphere) then
      Inc(Count);
  end;
  Result := ePartially;
  if Count = 0 then
    Result := eOutside
  else if Count = RealCount then
    Result:= eFully;
end;
(* End of PolyhedronInSphere *)


function PointOnPerimeter(const Px,Py,x1,y1,x2,y2:TGeoFloat):Boolean;
begin
 //Result := (((Px = x1) or (Px = x2)) and ((Py = y1) or (Py = y2)));
  Result := (
             ((IsEqual(Px,x1) or IsEqual(Px,x2)) and ((Py >= y1) and (Py <= y2))) or
             ((IsEqual(Py,y1) or IsEqual(Py,y2)) and ((Px >= x1) and (Px <= x2)))
            );
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Px,Py,x1,y1,x2,y2,x3,y3:TGeoFloat; Robust:Boolean = false):Boolean;
begin
  Result := (
             IsPointCollinear(x1,y1,x2,y2,Px,Py,Robust) or
             IsPointCollinear(x2,y2,x3,y3,Px,Py,Robust) or
             IsPointCollinear(x3,y3,x1,y1,Px,Py,Robust)
            );
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Px,Py,x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):Boolean;
begin
  Result := (
             IsPointCollinear(x1,y1,x2,y2,Px,Py) or
             IsPointCollinear(x2,y2,x3,y3,Px,Py) or
             IsPointCollinear(x3,y3,x4,y4,Px,Py) or
             IsPointCollinear(x4,y4,x1,y1,Px,Py)
            );
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TGeoPoint2D; const Rectangle:TGeoRectangle):Boolean;
begin
  Result := PointOnPerimeter(Point.x,Point.y,Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TGeoPoint2D; const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := PointOnPerimeter(Point.x,Point.y,Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y);
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TGeoPoint2D; const Quadix: TQuadix2D):Boolean;
begin
  Result := PointOnPerimeter(Point.x,Point.y,Quadix[1].x,Quadix[1].y,Quadix[2].x,Quadix[2].y,Quadix[3].x,Quadix[3].y,Quadix[4].x,Quadix[4].y);
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TGeoPoint2D; const Circle: TGeoCircle):Boolean;
begin
  //Result := (LayDistance(Point.x,Point.y,Circle.x,Circle.y) = (Circle.Radius * Circle.Radius));
  Result := IsEqual(LayDistance(Point.x,Point.y,Circle.x,Circle.y),(Circle.Radius * Circle.Radius));
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TGeoPoint3D; const Sphere: TGeoSphere):Boolean;
begin
  //Result := (LayDistance(Point.x,Point.y,Point.z,Sphr.x,Sphr.y,Sphr.z) = (Sphr.Radius * Sphr.Radius));
  Result := IsEqual(LayDistance(Point.x,Point.y,Point.z,Sphere.x,Sphere.y,Sphere.z),(Sphere.Radius * Sphere.Radius));
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point:TGeoPoint2D; const Polygon : TGeoPolygon2D):Boolean;
begin
  Result := PointOnPolygon(Point.x,Point.y,Polygon);
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TGeoPoint2D; const Obj:TGeometricObject):Boolean;
begin
  case Obj.ObjectType of
    geoRectangle  : Result := PointOnPerimeter(Point,Obj.Rectangle );
    geoTriangle2D : Result := PointOnPerimeter(Point,Obj.Triangle2D);
    geoQuadix2D   : Result := PointOnPerimeter(Point,Obj.Quadix2D  );
    geoCircle     : Result := PointOnPerimeter(Point,Obj.Circle    );
  else
    Result := False;
  end;
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TGeoPoint3D; const Obj:TGeometricObject):Boolean;
begin
  case Obj.ObjectType of
    geoSphere  : Result := PointOnPerimeter(Point,Obj.Sphere);
  else
    Result := False;
  end;
end;
(* End of PointOnPerimeter *)


function PointInObject(const Point:TGeoPoint2D; const Segment:TGeoSegment2D):Boolean;
begin
  Result := IsPointCollinear(Segment,Point);
end;
(* End of Point In Object *)


function PointInObject(const Point:TGeoPoint2D; const Line:TGeoLine2D):Boolean;
begin
  Result := Collinear(Line[1],Line[2],Point);
end;
(* End of Point In Object *)


function PointInObject(const Point:TGeoPoint2D; const Rectangle:TGeoRectangle):Boolean;
begin
  Result := PointInRectangle(Point,Rectangle);
end;
(* End of Point In Object *)


function PointInObject(const Point:TGeoPoint2D; const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := PointInTriangle(Point,Triangle);
end;
(* End of Point In Object *)


function PointInObject(const Point:TGeoPoint2D; const Quadix:TQuadix2D):Boolean;
begin
  Result := PointInQuadix(Point,Quadix);
end;
(* End of Point In Object *)


function PointInObject(const Point:TGeoPoint2D; const Circle:TGeoCircle):Boolean;
begin
  Result := PointInCircle(Point,Circle);
end;
(* End of Point In Object *)


function PointInObject(const Point:TGeoPoint2D; const Polygon:TGeoPolygon2D ):Boolean;
begin
  Result := PointInPolygon(Point,Polygon);
end;
(* End of Point In Object *)


function PointInObject(const Point:TGeoPoint2D; const Obj : TGeometricObject):Boolean;
begin
  case Obj.ObjectType of
    geoSegment2D  : Result := PointInObject(Point,Obj.Segment2D);
    geoLine2D     : Result := PointInObject(Point,Obj.Line2D);
    geoRectangle  : Result := PointInObject(Point,Obj.Rectangle);
    geoTriangle2D : Result := PointInObject(Point,Obj.Triangle2D);
    geoQuadix2D   : Result := PointInObject(Point,Obj.Quadix2D);
    geoCircle     : Result := PointInObject(Point,Obj.Circle);
    geoPolygon2D  : Result := PointInObject(Point,Obj.Polygon2D^);
  else
    Result := False;
  end;
end;
(* End of Point In Object *)


function GeometricSpan(const Point: array of TGeoPoint2D):TGeoFloat;
var
  TempDistance : TGeoFloat;
  i            : Integer;
  j            : Integer;
begin
  Result := -1;
  for i := 0 to Length(Point) - 2 do
  begin
    for j:= (i + 1) to Length(Point) - 1 do
    begin
      TempDistance := LayDistance(Point[i],Point[j]);
      if TempDistance > Result then
        Result := TempDistance;
    end;
  end;
  Result := Sqrt(Result);
end;
(* End of 2D Geometric Span *)


function GeometricSpan(const Point: array of TGeoPoint3D):TGeoFloat;
var
  TempDistance : TGeoFloat;
  i            : Integer;
  j            : Integer;
begin
  Result := -1;
  if Length(Point) < 2 then Exit;
  for i := 0 to Length(Point) - 2 do
  begin
    for j := (i + 1) to Length(Point) - 1 do
    begin
      TempDistance := LayDistance(Point[I],Point[J]);
      if TempDistance > Result then
        Result := TempDistance;
    end;
  end;
  Result := Sqrt(Result);
end;
(* End of 3D Geometric Span *)


procedure CreateEquilateralTriangle(x1,y1,x2,y2:TGeoFloat; out x3,y3:TGeoFloat);
const Sin60 : TGeoFloat = 0.86602540378443864676372317075294;
const Cos60 : TGeoFloat = 0.50000000000000000000000000000000;
begin
  (* Translate for x1,y1 to be origin *)
  x2 := x2 - x1;
  y2 := y2 - y1;
  (* Rotate 60 degrees and translate back *)
  x3 := ((x2 * Cos60) - (y2 * Sin60)) + x1;
  y3 := ((y2 * Cos60) + (x2 * Sin60)) + y1;
end;
(* End of Create Equilateral Triangle *)


procedure CreateEquilateralTriangle(const Point1,Point2:TGeoPoint2D; out Point3:TGeoPoint2D);
begin
  CreateEquilateralTriangle(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of Create Equilateral Triangle *)


function CreateEquilateralTriangle(const x1,y1,x2,y2:TGeoFloat):TGeoTriangle2D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[2].x := x2;
  Result[2].y := y2;
  CreateEquilateralTriangle(x1,y1,x2,y2,Result[3].x,Result[3].y);
end;
(* End of Create Equilateral Triangle *)


function CreateEquilateralTriangle(const Point1,Point2:TGeoPoint2D):TGeoTriangle2D;
begin
  Result[1] := Point1;
  Result[2] := Point2;
  CreateEquilateralTriangle(Result[1],Result[2],Result[3]);
end;
(* End of Create Equilateral Triangle *)


function CreateEquilateralTriangle(const Cx,Cy,SideLength : TGeoFloat) : TGeoTriangle2D;
begin
  Result := CenterAtLocation(CreateEquilateralTriangle(-SideLength * 0.5 , Zero, SideLength * 0.5, Zero),Cx,Cy);
end;
(* End of Create Equilateral Triangle *)


function CreateEquilateralTriangle(const CenterPoint : TGeoPoint2D; const SideLength : TGeoFloat) : TGeoTriangle2D;
begin
  Result := CreateEquilateralTriangle(CenterPoint.x, CenterPoint.y, SideLength);
end;
(* End of Create Equilateral Triangle *)


procedure TorricelliPoint(const x1,y1,x2,y2,x3,y3:TGeoFloat; out Px,Py:TGeoFloat);
var
  OETx1 : TGeoFloat;
  OETy1 : TGeoFloat;
  OETx2 : TGeoFloat;
  OETy2 : TGeoFloat;
begin
  (*
    Proven by Cavalieri in this book "Exercitationes geometricae" 1647.
    The theory goes, if the triangle has an angle of 120 degrees or more
    the toricelli point lies at the vertex of the large angle. Otherwise
    the point a which the Simpson lines intersect is said to be the optimal
    solution.
    to find an intersection in 2D, all that is needed is 2 lines (segments),
    hence not all three of the Simpson lines are calculated.
  *)
  //if VertexAngle(x1,y1,x2,y2,x3,y3) >= 120.0 then
  if GreaterThanOrEqual(VertexAngle(x1,y1,x2,y2,x3,y3),120.0) then
  begin
    Px := x2;
    Py := y2;
    Exit;
  end
  //else if VertexAngle(x3,y3,x1,y1,x2,y2) >= 120.0 then
  else if GreaterThanOrEqual(VertexAngle(x3,y3,x1,y1,x2,y2),120.0) then
  begin
    Px := x1;
    Py := y1;
    Exit;
  end
  //else if VertexAngle(x2,y2,x3,y3,x1,y1) >= 120.0 then
  else if GreaterThanOrEqual(VertexAngle(x2,y2,x3,y3,x1,y1),120.0) then
  begin
    Px := x3;
    Py := y3;
    Exit;
  end
  else
  begin
    if Orientation(x1,y1,x2,y2,x3,y3) = geoRightHandSide then
    begin
      CreateEquilateralTriangle(x1,y1,x2,y2,OETx1,OETy1);
      CreateEquilateralTriangle(x2,y2,x3,y3,OETx2,OETy2);
    end
    else
    begin
      CreateEquilateralTriangle(x2,y2,x1,y1,OETx1,OETy1);
      CreateEquilateralTriangle(x3,y3,x2,y2,OETx2,OETy2);
    end;
    IntersectionPoint(OETx1,OETy1,x3,y3,OETx2,OETy2,x1,y1,Px,Py);
  end;
end;
(* End of Create Torricelli Point *)


function TorricelliPoint(const Point1,Point2,Point3:TGeoPoint2D):TGeoPoint2D;
begin
  TorricelliPoint(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Result.x,Result.y);
end;
(* End of Create Torricelli Point *)


function TorricelliPoint(const Triangle:TGeoTriangle2D):TGeoPoint2D;
begin
  Result := TorricelliPoint(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of Create Torricelli Point *)


procedure Incenter(const x1,y1,x2,y2,x3,y3:TGeoFloat; out Px,Py:TGeoFloat);
var
  Perim  : TGeoFloat;
  Side12 : TGeoFloat;
  Side23 : TGeoFloat;
  Side31 : TGeoFloat;
begin
  Side12 := Distance(x1,y1,x2,y2);
  Side23 := Distance(x2,y2,x3,y3);
  Side31 := Distance(x3,y3,x1,y1);

  (* Using Heron's S=UR *)
  Perim  := 1.0 / (Side12 + Side23 + Side31);
  Px     := (Side23 * x1 + Side31 * x2 + Side12 * x3) * Perim;
  Py     := (Side23 * y1 + Side31 * y2 + Side12 * y3) * Perim;
end;
(* End of Incenter *)


procedure Incenter(const Triangle:TGeoTriangle2D; out Px,Py:TGeoFloat);
begin
  Incenter(Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y,Px,Py);
end;
(* End of Incenter *)


function Incenter(const Point1,Point2,Point3:TGeoPoint2D):TGeoPoint2D;
begin
  Incenter(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Result.x,Result.y);
end;
(* End of Incenter *)


function Incenter(const Triangle:TGeoTriangle2D):TGeoPoint2D;
begin
  Incenter(Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y,Result.x,Result.y);
end;
(* End of Incenter *)


procedure Circumcenter(const x1,y1,x2,y2,x3,y3:TGeoFloat; out Px,Py:TGeoFloat);
var
  A : TGeoFloat;
  C : TGeoFloat;
  B : TGeoFloat;
  D : TGeoFloat;
  E : TGeoFloat;
  F : TGeoFloat;
  G : TGeoFloat;
begin
  A := x2 - x1;
  B := y2 - y1;
  C := x3 - x1;
  D := y3 - y1;
  E := A * (x1 + x2) + B * (y1 + y2);
  F := C * (x1 + x3) + D * (y1 + y3);
  G := 2.0 * (A * (y3 - y2) - B * (x3 - x2));
  if IsEqual(G,Zero) then Exit;
  Px := (D * E - B * F) / G;
  Py := (A * F - C * E) / G;
end;
(* End of Circumcenter *)


function Circumcenter(const Point1,Point2,Point3:TGeoPoint2D):TGeoPoint2D;
begin
  Circumcenter(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Result.x,Result.y);
end;
(* End of Circumcenter *)


function Circumcenter(const Triangle:TGeoTriangle2D):TGeoPoint2D;
begin
  Circumcenter(Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y,Result.x,Result.y);
end;
(* End of Circumcenter *)


function Circumcircle(const P1,P2,P3:TGeoPoint2D):TGeoCircle;
begin
  Circumcenter(P1.x,P1.y,P2.x,P2.y,P3.x,P3.y,Result.x,Result.y);
  Result.Radius := Distance(P1.x,P1.y,Result.x,Result.y);
end;
(* End of TriangleCircumCircle *)


function Circumcircle(const Triangle:TGeoTriangle2D):TGeoCircle;
begin
  Result := CircumCircle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of TriangleCircumCircle *)


function InscribedCircle(const x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoCircle;
var
  Perimeter : TGeoFloat;
  Side12    : TGeoFloat;
  Side23    : TGeoFloat;
  Side31    : TGeoFloat;
begin
  Side12 := Distance(x1,y1,x2,y2);
  Side23 := Distance(x2,y2,x3,y3);
  Side31 := Distance(x3,y3,x1,y1);

  (* Using Heron's S = UR *)
  Perimeter     := 1.0 / (Side12 + Side23 + Side31);
  Result.x      := (Side23 * x1 + Side31 * x2 + Side12 * x3) * Perimeter;
  Result.y      := (Side23 * y1 + Side31 * y2 + Side12 * y3) * Perimeter;
  Result.Radius := 0.5 * sqrt((-Side12 + Side23 + Side31) * (Side12 - Side23 + Side31) * (Side12 + Side23 - Side31) * Perimeter);
end;
(* End of InscribedCircle *)


function InscribedCircle(const P1,P2,P3:TGeoPoint2D):TGeoCircle;
begin
  Result := InscribedCircle(P1.x,P1.y,P2.x,P2.y,P3.x,P3.y);
end;
(* End of InscribedCircle *)


function InscribedCircle(const Triangle:TGeoTriangle2D):TGeoCircle;
begin
  Result := InscribedCircle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of InscribedCircle *)


procedure ClosestPointOnSegmentFromPoint(const x1,y1,x2,y2,Px,Py:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  Vx    : TGeoFloat;
  Vy    : TGeoFloat;
  Wx    : TGeoFloat;
  Wy    : TGeoFloat;
  c1    : TGeoFloat;
  c2    : TGeoFloat;
  Ratio : TGeoFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Wx := Px - x1;
  Wy := Py - y1;

  c1 := Vx * Wx + Vy * Wy;

  if c1 <= 0.0 then
  begin
    Nx := x1;
    Ny := y1;
    Exit;
  end;

  c2 := Vx * Vx + Vy * Vy;

  if c2 <= c1  then
  begin
    Nx := x2;
    Ny := y2;
    Exit;
  end;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
end;
(* End of ClosestPointOnSegmentFromPoint *)


procedure ClosestPointOnSegmentFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);
var
  Vx    : TGeoFloat;
  Vy    : TGeoFloat;
  Vz    : TGeoFloat;
  Wx    : TGeoFloat;
  Wy    : TGeoFloat;
  wz    : TGeoFloat;
  c1    : TGeoFloat;
  c2    : TGeoFloat;
  Ratio : TGeoFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Vz := z2 - z1;
  Wx := Px - x1;
  Wy := Py - y1;
  wz := Pz - z1;

  c1 := Vx * Wx + Vy * Wy + Vz * Wz;

  if c1 <= 0.0 then
  begin
    Nx := x1;
    Ny := y1;
    Nz := z1;
    Exit;
  end;

  c2 := Vx * Vx + Vy * Vy + Vz * Vz;

  if c2 <= c1  then
  begin
    Nx := x2;
    Ny := y2;
    Nz := z2;
    Exit;
  end;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
  Nz := z1 + Ratio * Vz;
end;
(* End of ClosestPointOnSegmentFromPoint *)


procedure ClosestPointOnLineFromPoint(const x1,y1,x2,y2,Px,Py:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  Vx    : TGeoFloat;
  Vy    : TGeoFloat;
  Wx    : TGeoFloat;
  Wy    : TGeoFloat;
  c1    : TGeoFloat;
  c2    : TGeoFloat;
  Ratio : TGeoFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Wx := Px - x1;
  Wy := Py - y1;

  c1 := Vx * Wx + Vy * Wy;
  c2 := Vx * Vx + Vy * Vy;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
end;
(* End of ClosestPointOnLineFromPoint *)


procedure ClosestPointOnLineFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);
var
  Vx    : TGeoFloat;
  Vy    : TGeoFloat;
  Vz    : TGeoFloat;
  Wx    : TGeoFloat;
  Wy    : TGeoFloat;
  wz    : TGeoFloat;
  c1    : TGeoFloat;
  c2    : TGeoFloat;
  Ratio : TGeoFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Vz := z2 - z1;
  Wx := Px - x1;
  Wy := Py - y1;
  wz := Pz - z1;

  c1 := Vx * Wx + Vy * Wy + Vz * Wz;
  c2 := Vx * Vx + Vy * Vy + Vz * Vz;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
  Nz := z1 + Ratio * Vz;
end;
(* End of ClosestPointOnLineFromPoint *)


function ClosestPointOnSegmentFromPoint(const x1,y1,x2,y2,Px,Py:TGeoFloat):TGeoPoint2D;
begin
  ClosestPointOnSegmentFromPoint(x1,y1,x2,y2,Px,Py,Result.x,Result.y);
end;
(* End of ClosestPointOnSegmentFromPoint *)


function ClosestPointOnSegmentFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TGeoFloat):TGeoPoint3D;
begin
  ClosestPointOnSegmentFromPoint(x1,y1,z1,x2,y2,z2,Px,Py,Pz,Result.x,Result.y,Result.z);
end;
(* End of ClosestPointOnSegmentFromPoint *)


function ClosestPointOnSegmentFromPoint(const Segment:TGeoSegment2D; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  ClosestPointOnSegmentFromPoint(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Point.x,Point.y,Result.x,Result.y);
end;
(* End of ClosestPointOnSegmentFromPoint *)


function ClosestPointOnSegmentFromPoint(const Segment:TGeoSegment3D; const Point:TGeoPoint3D):TGeoPoint3D;
begin
  ClosestPointOnSegmentFromPoint(Segment[1].x,Segment[1].y,Segment[1].z,Segment[2].x,Segment[2].y,Segment[2].z,Point.x,Point.y,Point.z,Result.x,Result.y,Result.Z);
end;
(* End of ClosestPointOnSegmentFromPoint *)


function ClosestPointOnLineFromPoint(const x1,y1,x2,y2,Px,Py:TGeoFloat):TGeoPoint2D;
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,Px,Py,Result.x,Result.y)
end;
(* End of ClosestPointOnLineFromPoint *)


function ClosestPointOnLineFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TGeoFloat):TGeoPoint3D;
begin
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,Px,Py,Pz,Result.x,Result.y,Result.z);
end;
(* End of ClosestPointOnLineFromPoint *)


function ClosestPointOnLineFromPoint(const Line:TGeoLine2D; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  ClosestPointOnLineFromPoint(Line[1].x,Line[1].y,Line[2].x,Line[2].y,Point.x,Point.y,Result.x,Result.y);
end;
(* End of ClosestPointOnLineFromPoint *)


function ClosestPointOnLineFromPoint(const Line:TGeoLine3D; const Point:TGeoPoint3D):TGeoPoint3D;
begin
  ClosestPointOnLineFromPoint(Line[1].x,Line[1].y,Line[1].z,Line[2].x,Line[2].y,Line[2].z,Point.x,Point.y,Point.z,Result.x,Result.y,Result.Z);
end;
(* End of ClosestPointOnLineFromPoint *)


procedure ClosestPointOnTriangleFromPoint(const x1,y1,x2,y2,x3,y3,Px,Py:TGeoFloat; out Nx,Ny : TGeoFloat);
begin
  if Orientation(x1,y1,x2,y2,Px,Py) <> Orientation(x1,y1,x2,y2,x3,y3) then
  begin
    ClosestPointOnSegmentFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
    Exit;
  end;

  if Orientation(x2,y2,x3,y3,Px,Py) <> Orientation(x2,y2,x3,y3,x1,y1) then
  begin
    ClosestPointOnSegmentFromPoint(x2,y2,x3,y3,Px,Py,Nx,Ny);
    Exit;
  end;

  if Orientation(x3,y3,x1,y1,Px,Py) <> Orientation(x3,y3,x1,y1,x2,y2) then
  begin
    ClosestPointOnSegmentFromPoint(x3,y3,x1,y1,Px,Py,Nx,Ny);
    Exit;
  end;
  Nx := Px;
  Ny := Py;
end;
(* Closest Point On Triangle 2D From Point *)


function ClosestPointOnTriangleFromPoint(const Triangle:TGeoTriangle2D; const Px,Py:TGeoFloat):TGeoPoint2D;
begin
  ClosestPointOnTriangleFromPoint(Triangle[1].x, Triangle[1].y,
                                  Triangle[2].x, Triangle[2].y,
                                  Triangle[3].x, Triangle[3].y,
                                  Px           , Py           ,
                                  Result.x     , Result.y
                                 );
end;
(* Closest Point On Triangle 2D From Point *)


function ClosestPointOnTriangleFromPoint(const Triangle:TGeoTriangle2D; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  Result := ClosestPointOnTriangleFromPoint(Triangle,Point.x,Point.y);
end;
(* Closest Point On Triangle 2D From Point *)


function ClosestPointOnTriangleFromPoint(const Triangle:TGeoTriangle3D; const Point:TGeoPoint3D):TGeoPoint3D;
var
  Leydist1 : TGeoFloat;
  Leydist2 : TGeoFloat;
  Leydist3 : TGeoFloat;
begin
  Leydist1 := LayDistance(Triangle[1],Point);
  Leydist2 := LayDistance(Triangle[2],Point);
  Leydist3 := LayDistance(Triangle[3],Point);

  (* Edge 3 is the longest *)
  if GreaterThanOrEqual(Leydist3,Leydist1) and GreaterThanOrEqual(Leydist3,Leydist2) then
  begin
    ClosestPointOnSegmentFromPoint(Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Point.x,Point.y,Result.x,Result.y);
    Exit;
  end;

  (* Edge 2 is the longest *)
  if GreaterThanOrEqual(Leydist2,Leydist1) and GreaterThanOrEqual(Leydist2,Leydist3) then
  begin
    ClosestPointOnSegmentFromPoint(Triangle[1].x,Triangle[1].y,Triangle[3].x,Triangle[3].y,Point.x,Point.y,Result.x,Result.y);
    Exit;
  end;

  (* Edge 1 is the longest *)
  if GreaterThanOrEqual(Leydist1,Leydist2) and GreaterThanOrEqual(Leydist1,Leydist3) then
  begin
    ClosestPointOnSegmentFromPoint(Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y,Point.x,Point.y,Result.x,Result.y);
    Exit;
  end;

end;
(* Closest Point On Triangle 3D From Point *)


procedure ClosestPointOnRectangleFromPoint(const x1,y1,x2,y2,Px,Py:TGeoFloat; out Nx,Ny:TGeoFloat);
begin
  if (Px < Min(x1,x2)) then
    Nx := Min(x1,x2)
  else if (Px > Max(x1,x2)) then
    Nx := Max(x1,x2)
  else
    Nx := Px;

  if (Py < Min(y1,y2)) then
    Ny := Min(y1,y2)
  else if (Py > Max(y1,y2)) then
    Ny := Max(y1,y2)
  else
    Ny := Py;
end;
(* Closest Point On Rectangle From Point *)


function ClosestPointOnRectangleFromPoint(const Rectangle:TGeoRectangle; const Px,Py:TGeoFloat):TGeoPoint2D;
begin
  ClosestPointOnRectangleFromPoint(Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y,Px,Py,Result.x,Result.y);
end;
(* Closest Point On Rectangle From Point *)


function ClosestPointOnRectangleFromPoint(const Rectangle:TGeoRectangle; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  Result := ClosestPointOnRectangleFromPoint(Rectangle,Point.x,Point.y);
end;
(* Closest Point On Rectangle From Point *)


function ClosestPointOnQuadixFromPoint(const Quadix:TQuadix2D; const Point:TGeoPoint2D):TGeoPoint2D;
var
  MinDist   : TGeoFloat;
  TempDist  : TGeoFloat;
  TempPoint : TGeoPoint2D;
begin
  if PointInQuadix(Point,Quadix) then
  begin
    Result := Point;
    Exit;
  end;

  ClosestPointOnSegmentFromPoint(Quadix[1].x,Quadix[1].y,Quadix[2].x,Quadix[2].y,Point.x,Point.y,Result.x,Result.y);
  MinDist := Distance(Result,Point);

  ClosestPointOnSegmentFromPoint(Quadix[2].x,Quadix[2].y,Quadix[3].x,Quadix[3].y,Point.x,Point.y,TempPoint.x,TempPoint.y);
  TempDist := Distance(TempPoint,Point);

  if MinDist > TempDist then
  begin
    MinDist := TempDist;
    Result  := TempPoint;
  end;

  ClosestPointOnSegmentFromPoint(Quadix[3].x,Quadix[3].y,Quadix[4].x,Quadix[4].y,Point.x,Point.y,TempPoint.x,TempPoint.y);
  TempDist := Distance(TempPoint,Point);

  if MinDist > TempDist then
  begin
    MinDist := TempDist;
    Result  := TempPoint;
  end;

  ClosestPointOnSegmentFromPoint(Quadix[4].x,Quadix[4].y,Quadix[1].x,Quadix[1].y,Point.x,Point.y,TempPoint.x,TempPoint.y);
  TempDist := Distance(TempPoint,Point);

  if MinDist > TempDist then
  begin
    Result  := TempPoint;
  end;
end;
(* Closest Point On Quadix 2D From Point *)


function ClosestPointOnQuadixFromPoint(const Quadix:TQuadix3D; const Point:TGeoPoint3D):TGeoPoint3D;
(*
var
  Leydist1 : TGeoFloat;
  Leydist2 : TGeoFloat;
  Leydist3 : TGeoFloat;
  Leydist4 : TGeoFloat;
*)
begin
 (*
  Leydist1 := LayDistance(Quadix[1],Point);
  Leydist2 := LayDistance(Quadix[2],Point);
  Leydist3 := LayDistance(Quadix[3],Point);
  Leydist4 := LayDistance(Quadix[4],Point);
 *)
end;
(* Closest Point On Quadix 3D From Point *)


function ClosestPointOnCircleFromPoint(const Circle:TGeoCircle; const Point:TGeoPoint2D):TGeoPoint2D;
var
  Ratio : TGeoFloat;
  dx    : TGeoFloat;
  dy    : TGeoFloat;
begin
  dx       := Point.x - Circle.x;
  dy       := Point.y - Circle.y;
  Ratio    := Circle.Radius / sqrt(dx * dx + dy * dy);
  Result.x := Circle.x + Ratio * dx;
  Result.y := Circle.y + Ratio * dy;
end;
(* Closest Point On Circle From Point *)


function ClosestPointOnSphereFromPoint(const Sphere:TGeoSphere; const Point:TGeoPoint3D):TGeoPoint3D;
var
  Ratio : TGeoFloat;
  dx    : TGeoFloat;
  dy    : TGeoFloat;
  dz    : TGeoFloat;
begin
  dx       := Point.x - Sphere.x;
  dy       := Point.y - Sphere.y;
  dz       := Point.z - Sphere.z;
  Ratio    := Sphere.Radius / sqrt(dx * dx + dy * dy + dz * dz);
  Result.x := Sphere.x + Ratio * dx;
  Result.y := Sphere.y + Ratio * dy;
  Result.z := Sphere.z + Ratio * dz;
end;
(* Closest Point On Sphere From Point *)


function ClosestPointOnAABBFromPoint(const Rectangle: TGeoRectangle; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  Result := Point;
  if Point.x <= Rectangle[1].x then
    Result.x := Rectangle[1].x
  else if Point.x >= Rectangle[2].x then
    Result.x := Rectangle[2].x;

  if Point.y <= Rectangle[1].y then
    Result.y := Rectangle[1].y
  else if Point.y >= Rectangle[2].y then
    Result.y := Rectangle[2].y;
end;
(* Closest Point On AABB From Point *)


function ClosestPointOnCircleFromSegment(const Circle:TGeoCircle; Segment:TGeoSegment2D):TGeoPoint2D;
var
  Nx    : TGeoFloat;
  Ny    : TGeoFloat;
  Ratio : TGeoFloat;
begin
  ClosestPointOnSegmentFromPoint(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Circle.x,Circle.y,Nx,Ny);
  Ratio    := Circle.Radius / Distance(Circle.x,Circle.y,Nx,Ny);
  Result.x := Circle.x + Ratio * (Nx - Circle.x);
  Result.y := Circle.y + Ratio * (Ny - Circle.y);
end;
(* End of ClosestPointOnCircle *)


function ClosestPointOnSphereFromSegment(const Sphere:TGeoSphere; Segment:TGeoSegment3D):TGeoPoint3D;
var
  Nx    : TGeoFloat;
  Ny    : TGeoFloat;
  Nz    : TGeoFloat;
  Ratio : TGeoFloat;
begin
  ClosestPointOnSegmentFromPoint(Segment[1].x,Segment[1].y,Segment[1].z,Segment[2].x,Segment[2].y,Segment[2].z,Sphere.x,Sphere.y,Sphere.z,Nx,Ny,Nz);
  Ratio    := Sphere.Radius / Distance(Sphere.x,Sphere.y,Sphere.z,Nx,Ny,Nz);
  Result.x := Sphere.x + Ratio * (Nx - Sphere.x);
  Result.y := Sphere.y + Ratio * (Ny - Sphere.y);
  Result.z := Sphere.z + Ratio * (Nz - Sphere.z);
end;
(* End of ClosestPointOnCircle *)


function MinimumDistanceFromPointToSegment(const Px,Py,x1,y1,x2,y2:TGeoFloat):TGeoFloat;
var
  Nx    : TGeoFloat;
  Ny    : TGeoFloat;
begin
  ClosestPointOnSegmentFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
  Result := Distance(Px,Py,Nx,Ny);
end;
(* End of Minimum Distance From Point to Segment *)


function MinimumDistanceFromPointToSegment(const Point:TGeoPoint2D; const Segment:TGeoSegment2D):TGeoFloat;
begin
  Result := MinimumDistanceFromPointToSegment(Point.x,Point.y,Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y);
end;
(* End of Minimum Distance From Point to Segment *)


function MinimumDistanceFromPointToSegment(const Px,Py,Pz,x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;
var
  Nx    : TGeoFloat;
  Ny    : TGeoFloat;
  Nz    : TGeoFloat;
begin
  ClosestPointOnSegmentFromPoint(x1,y1,z1,x2,y2,z2,Px,Py,Pz,Nx,Ny,Nz);
  Result := Distance(Px,Py,Pz,Nx,Ny,Nz);
end;
(* End of Minimum Distance From Point to Segment *)


function MinimumDistanceFromPointToSegment(const Point:TGeoPoint3D; const Segment:TGeoSegment3D):TGeoFloat;
begin
  Result := MinimumDistanceFromPointToSegment(Point.x,Point.y,Point.z,Segment[1].x,Segment[1].y,Segment[1].z,Segment[2].x,Segment[2].y,Segment[2].z);
end;
(* End of Minimum Distance From Point to Segment *)


function MinimumDistanceFromPointToLine(const Px,Py,x1,y1,x2,y2:TGeoFloat):TGeoFloat;
var
  Nx : TGeoFloat;
  Ny : TGeoFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
  Result := Distance(Px,Py,Nx,Ny);
end;
(* End of Minimum Distance From Point to Line *)


function MinimumDistanceFromPointToLine(const Point:TGeoPoint2D; const Line:TGeoLine2D):TGeoFloat;
begin
  Result := MinimumDistanceFromPointToLine(Point.x,Point.y, Line[1].x,Line[1].y,Line[2].x,Line[2].y);
end;
(* End of Minimum Distance From Point to Line *)


function MinimumDistanceFromPointToLine(const Px,Py,Pz,x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoFloat;
var
  Nx : TGeoFloat;
  Ny : TGeoFloat;
  Nz : TGeoFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,Px,Py,Pz,Nx,Ny,Nz);
  Result := Distance(Px,Py,Pz,Nx,Ny,Nz);
end;
(* End of Minimum Distance From Point to Line *)


function MinimumDistanceFromPointToLine(const Point:TGeoPoint3D; const Line:TGeoLine3D):TGeoFloat;
begin
  Result := MinimumDistanceFromPointToLine(Point.x,Point.y,Point.z,Line[1].x,Line[1].y,Line[1].z,Line[2].x,Line[2].y,Line[2].z);
end;
(* End of Minimum Distance From Point to Line *)


function MinimumDistanceFromPointToTriangle(const Px,Py,x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoFloat;
var
  Nx : TGeoFloat;
  Ny : TGeoFloat;
begin
  ClosestPointOnTriangleFromPoint(x1,y1,x2,y2,x3,y3,Px,Py,Nx,Ny);
  Result := Distance(Px,Py,Nx,Ny);
end;
(* End of Minimum Distance From Point to Triangle *)


function MinimumDistanceFromPointToTriangle(const Point:TGeoPoint2D; const Triangle:TGeoTriangle2D):TGeoFloat;
begin
  Result := MinimumDistanceFromPointToTriangle(Point.x,Point.y, Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y);
end;
(* End of Minimum Distance From Point to Triangle *)


function MinimumDistanceFromPointToRectangle(const Px,Py,x1,y1,x2,y2:TGeoFloat):TGeoFloat;
var
  Nx : TGeoFloat;
  Ny : TGeoFloat;
begin
  ClosestPointOnRectangleFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
  Result := Distance(Px,Py,Nx,Ny);
end;
(* End of Minimum Distance From Point to Rectangle *)


function MinimumDistanceFromPointToRectangle(const Point:TGeoPoint2D; const Rectangle:TGeoRectangle):TGeoFloat;
begin
  Result := MinimumDistanceFromPointToRectangle(Point.x,Point.y,Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
end;
(* End of Minimum Distance From Point to Rectangle *)


function MinimumDistanceFromPointToPolygon(const Point:TGeoPoint2D; const Polygon:TGeoPolygon2D):TGeoFloat;
var
  i        : Integer;
  j        : Integer;
  TempDist : TGeoFloat;
begin
  Result := Zero;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  i := 0;
  Result := MinimumDistanceFromPointToSegment(Point.x,Point.y,Polygon[i].x,Polygon[i].y,Polygon[j].x,Polygon[j].y);
  j := 0;
  for i := 1 to Length(Polygon) - 1 do
  begin
    TempDist := MinimumDistanceFromPointToSegment(Point.x,Point.y,Polygon[i].x,Polygon[i].y,Polygon[j].x,Polygon[j].y);
    if TempDist < Result then
      Result := TempDist;
    j := i;
  end;
end;
(* End of Minimum Distance From Point to Line *)


procedure SegmentMidPoint(const x1,y1,x2,y2:TGeoFloat; out midx, midy:TGeoFloat);
begin
  midx := (x1 + x2) * 0.5;
  midy := (y1 + y2) * 0.5;
end;
(* End of SegmentMidPoint *)


procedure SegmentMidPoint(const Segment:TGeoSegment2D; out midx, midy:TGeoFloat);
begin
  SegmentMidPoint(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,midx,midy);
end;
(* End of SegmentMidPoint *)


function SegmentMidPoint(const P1,P2:TGeoPoint2D):TGeoPoint2D;
begin
  SegmentMidPoint(P1.x,P1.y,P2.x,P2.y,Result.x,Result.y);
end;
(* End of SegmentMidPoint *)


function SegmentMidPoint(const Segment:TGeoSegment2D):TGeoPoint2D;
begin
  Result := SegmentMidPoint(Segment[1],Segment[2]);
end;
(* End of SegmentMidPoint *)


procedure SegmentMidPoint(const x1,y1,z1,x2,y2,z2:TGeoFloat; out midx, midy ,midz:TGeoFloat);
begin
  midx := (x1 + x2) * 0.5;
  midy := (y1 + y2) * 0.5;
  midz := (z1 + z2) * 0.5;
end;
(* End of SegmentMidPoint *)


function SegmentMidPoint(const P1,P2:TGeoPoint3D):TGeoPoint3D;
begin
  SegmentMidpoint(P1.x,P1.y,P1.z,P2.x,P2.y,P2.z,Result.x,Result.y,Result.z);
end;
(* End of SegmentMidPoint *)


function SegmentMidPoint(const Segment:TGeoSegment3D):TGeoPoint3D;
begin
 Result := SegmentMidPoint(Segment[1],Segment[2]);
end;
(* End of SegmentMidPoint *)


procedure Centroid(const x1,y1,x2,y2:TGeoFloat; out x,y:TGeoFloat);
begin
  x := (x1 + x2) * 0.5;
  y := (y1 + y2) * 0.5;
end;
(* End of Centroid *)


function Centroid(const P1,P2:TGeoPoint2D):TGeoPoint2D;
begin
  Centroid(P1.x,P1.y,P2.x,P2.y,Result.x,Result.y);
end;
(* End of Centroid *)


function Centroid(const Segment:TGeoSegment2D):TGeoPoint2D;
begin
  Result := Centroid(Segment[1],Segment[2]);
end;
(* End of Centroid *)


procedure Centroid(const x1,y1,x2,y2,x3,y3:TGeoFloat; out x,y:TGeoFloat);
var
  midx1 : TGeoFloat;
  midy1 : TGeoFloat;
  midx2 : TGeoFloat;
  midy2 : TGeoFloat;
begin
  SegmentMidPoint(x2,y2,x3,y3,midx1,midy1);
  SegmentMidPoint(x1,y1,x3,y3,midx2,midy2);
  Intersect(x1,y1,midx1,midy1,x2,y2,midx2,midy2,x,y);
end;
(* End of Centroid *)


procedure Centroid(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; out x,y:TGeoFloat);
var
  asum : TGeoFloat;
  term : TGeoFloat;
begin
  x := 0.0;
  y := 0.0;

  asum := 0.0;

  term := ((x4 * y1) - (y4 * x1));
  asum := asum + term;
  x := x + (x4 + x1) * term;
  y := y + (y4 + y1) * term;

  term := ((x1 * y2) - (y1 * x2));
  asum := asum + term;
  x := x + (x1 + x2) * term;
  y := y + (y1 + y2) * term;

  term := ((x2 * y3) - (y2 * x3));
  asum := asum + term;
  x := x + (x2 + x3) * term;
  y := y + (y2 + y3) * term;

  term := ((x3 * y4) - (y3 * x4));
  asum := asum + term;
  x := x + (x3 + x4) * term;
  y := y + (y3 + y4) * term;

  if asum <> Zero then
  begin
    x := x / (3.0 * asum);
    y := y / (3.0 * asum);
  end;
end;
(* End of Centroid *)


procedure Centroid(const Triangle:TGeoTriangle2D; out x,y:TGeoFloat);
begin
  Centroid(Triangle[1].x,Triangle[1].y,
           Triangle[2].x,Triangle[2].y,
           Triangle[3].x,Triangle[3].y,x,y);
end;
(* End of Centroid *)


procedure Centroid(const Rectangle:TGeoRectangle; out x,y:TGeoFloat);
begin
  x := (Rectangle[1].x + Rectangle[2].x) * 0.5;
  y := (Rectangle[1].y + Rectangle[2].y) * 0.5;
end;
(* End of Centroid *)


procedure Centroid(const Quadix:TQuadix2D; out x,y:TGeoFloat);
begin
  Centroid(Quadix[1].x,Quadix[1].y,
           Quadix[2].x,Quadix[2].y,
           Quadix[3].x,Quadix[3].y,
           Quadix[4].x,Quadix[4].y,x,y);
end;
(* End of Centroid *)


function Centroid(const P1,P2,P3:TGeoPoint2D):TGeoPoint2D;
begin
  Centroid(P1.x,P1.y,P2.x,P2.y,P3.x,P3.y,Result.x,Result.y);
end;
(* End of Centroid *)


function Centroid(const P1,P2,P3,P4:TGeoPoint2D):TGeoPoint2D;
begin
  Centroid(P1.x,P1.y,P2.x,P2.y,P3.x,P3.y,P4.x,P4.y,Result.x,Result.y);
end;
(* End of Centroid *)


function Centroid(const Triangle:TGeoTriangle2D):TGeoPoint2D;
begin
  Result := Centroid(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of Centroid *)


function  Centroid(const Quadix:TQuadix2D):TGeoPoint2D;
begin
  Result := Centroid(Quadix[1],Quadix[2],Quadix[3],Quadix[4]);
end;
(* End of Centroid *)


function Centroid(const Rectangle:TGeoRectangle):TGeoPoint2D;
begin
  Centroid(Rectangle,Result.x,Result.y);
end;
(* End of Centroid *)


procedure Centroid(const Polygon:TGeoPolygon2D; out x,y:TGeoFloat);
var
  i    : Integer;
  j    : Integer;
  asum : TGeoFloat;
  term : TGeoFloat;
begin
  x := Zero;
  y := Zero;

  if Length(Polygon) < 3 then Exit;

  asum := Zero;
  j    := Length(Polygon) - 1;

  for i := 0 to Length(Polygon) - 1 do
  begin
    term := ((Polygon[j].x * Polygon[i].y) - (Polygon[j].y * Polygon[i].x));
    asum := asum + term;
    x := x + (Polygon[j].x + Polygon[i].x) * term;
    y := y + (Polygon[j].y + Polygon[i].y) * term;
    j := i;
  end;

  if NotEqual(asum,Zero) then
  begin
    x := x / (3.0 * asum);
    y := y / (3.0 * asum);
  end;
end;
(* End of PolygonCentroid *)


function Centroid(const Polygon:TGeoPolygon2D): TGeoPoint2D;
begin
  Centroid(Polygon,Result.x,Result.y);
end;
(* End of PolygonCentroid *)


function Centroid(const Polygon : array of TGeoPoint3D):TGeoPoint3D;
var
  TotalArea : TGeoFloat;
  TempArea  : TGeoFloat;
  i         : Integer;
  Len       : Integer;
begin
  Result.x := Zero;
  Result.y := Zero;
  Result.z := Zero;

  if Length(Polygon) < 3 then Exit;

  TotalArea := Zero;
  Len       := Length(Polygon);

  for i := 0 to Len - 2 do
  begin
    TempArea := Area(Polygon[i],Polygon[i + 1],Polygon[Len - 1]);

    TotalArea := TotalArea + TempArea;

    Result.x := Result.x + TempArea * (Polygon[i].x + Polygon[i + 1].x + Polygon[Len - 1].x ) / 3.0;
    Result.y := Result.y + TempArea * (Polygon[i].y + Polygon[i + 1].y + Polygon[Len - 1].y ) / 3.0;
    Result.z := Result.z + TempArea * (Polygon[i].z + Polygon[i + 1].z + Polygon[Len - 1].z ) / 3.0;
  end;

  Result.x := Result.x / TotalArea;
  Result.y := Result.y / TotalArea;
  Result.z := Result.z / TotalArea;
end;
(* End of PolygonCentroid *)


function PolygonSegmentIntersect(const Segment:TGeoSegment2D; const Polygon:TGeoPolygon2D):Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    if Intersect(Segment[1],Segment[2],Polygon[i],Polygon[j]) then
    begin
      Result := True;
      Break;
    end;
    j := i;
  end;
end;
(* End of PolygonSegmentIntersect *)


function PolygonInPolygon(const Poly1,Poly2: TGeoPolygon2D):Boolean;
begin
  (* to be implemented at a later date *)
  Result := False;
end;
(* End of PolygonInPolygon *)


function PointInConvexPolygon(const Px,Py:TGeoFloat; const Polygon:TGeoPolygon2D):Boolean;
var
  i                  : Integer;
  j                  : Integer;
  InitialOrientation : Integer;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  Result             := True;
  InitialOrientation := Orientation(Polygon[0],Polygon[Length(Polygon) - 1],Px,Py);
  j                  := 0;
  if InitialOrientation <> 0 then
  for i := 1 to Length(Polygon) - 1 do
  begin
    if InitialOrientation <> Orientation(Polygon[i],Polygon[j],Px,Py) then
    begin
      Result := False;
      Exit;
    end;
    j := i;
  end;
end;
(* End of PointInConvexPolygon *)


function PointInConvexPolygon(const Point:TGeoPoint2D; const Polygon:TGeoPolygon2D):Boolean;
begin
  Result := PointInConvexPolygon(Point.x,Point.y,Polygon);
end;
(* End of PointInConvexPolygon *)


function PointInConcavePolygon(const Px,Py:TGeoFloat; const Polygon:TGeoPolygon2D):Boolean;
begin
 (* to be implemented at a later date *)
  Result := False;
end;
(* End of PointInConcavePolygon *)


function PointInConcavePolygon(const Point:TGeoPoint2D; const Polygon:TGeoPolygon2D):Boolean;
begin
  Result := PointInConcavePolygon(Point.x,Point.y,Polygon);
end;
(* End of PointInConcavePolygon *)


function PointOnPolygon(const Px,Py:TGeoFloat; const Polygon:TGeoPolygon2D):Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    if ((Polygon[i].y <= Py) and (Py < Polygon[j].y)) or
       ((Polygon[j].y <= Py) and (Py < Polygon[i].y)) then
    begin
      if IsPointCollinear(Polygon[i],Polygon[j],Px,Py) then
      begin
        Result := True;
        Exit;
      end;
    end;
    j := i;
  end;
end;
(* End of PointOnPolygon *)


function PointOnPolygon(const Point:TGeoPoint2D; const Polygon:TGeoPolygon2D):Boolean;
begin
  Result := PointOnPolygon(Point.x,Point.y,Polygon);
end;
(* End of PointOnPolygon *)


function PointInPolygon(const Px,Py:TGeoFloat; const Polygon:TGeoPolygon2D):Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    if ((Polygon[i].y <= Py) and (Py < Polygon[j].y)) or    // an upward crossing
       ((Polygon[j].y <= Py) and (Py < Polygon[i].y)) then  // a downward crossing
    begin
      (* compute the edge-ray intersect @ the x-coordinate *)
      if (Px - Polygon[i].x < ((Polygon[j].x - Polygon[i].x) * (Py - Polygon[i].y) / (Polygon[j].y - Polygon[i].y))) then
        Result := not Result;
    end;
    j := i;
  end;
end;
(* End PointInPolygon *)


function PointInPolygon(const Point:TGeoPoint2D; const Polygon:TGeoPolygon2D):Boolean;
begin
  Result := PointInPolygon(Point.x,Point.y,Polygon);
end;
(* End PointInPolygon *)


function PointInPolygon(const Point: array of TGeoPoint2D; const Polygon:TGeoPolygon2D): TGeoBooleanArray;
var
  i               : Integer;
  j               : Integer;
  k               : Integer;
  Ratio           : TGeoFloat;
  RatioCalculated : Boolean;
begin
  Ratio           := 0;
  RatioCalculated := False;

  if (Length(Polygon) < 3) or (Length(Point) = 0) then Exit;
  j := Length(Polygon) - 1;

  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Result) - 1 do Result[i] := False;

  for i := 0 to Length(Polygon) - 1 do
  begin
    for k := 0 to length(Point) - 1 do
    if ((Polygon[i].y <= Point[k].y) and (Point[k].y < Polygon[j].y)) or   // an upward crossing
       ((Polygon[j].y <= Point[k].y) and (Point[k].y < Polygon[i].y)) then // a downward crossing
    begin
      if not RatioCalculated then
      begin
        Ratio           := (Polygon[j].x - Polygon[i].x) / (Polygon[j].y - Polygon[i].y);
        RatioCalculated := True;
      end;
      if (Point[k].x - Polygon[i].x < ((Point[k].y - Polygon[i].y) * Ratio)) then
      Result[k] := not Result[k];
    end;
    RatioCalculated := False;
    j := i;
  end;
end;
(* End PointInPolygon *)


function ConvexQuadix(const Quadix:TQuadix2D):Boolean;
var
  Orin : TGeoFloat;
begin
  Result := False;
  Orin   := Orientation(Quadix[1],Quadix[3],Quadix[2]);
  if Orin <> Orientation(Quadix[2],Quadix[4],Quadix[3]) then Exit;
  if Orin <> Orientation(Quadix[3],Quadix[1],Quadix[4]) then Exit;
  if Orin <> Orientation(Quadix[4],Quadix[2],Quadix[1]) then Exit;
  Result:= True;
end;
(* End of ConvexQuadix *)


function ComplexPolygon(const Polygon:TGeoPolygon2D):Boolean;
begin
  (*
    Complex polygon definition
    A polygon that can have:
    1.) Self intersecting edges
    2.) Holes
    3.) Unclosed area
  *)
  Result := SelfIntersectingPolygon(Polygon);
end;
(* End of ComplexPolygon *)


function SimplePolygon(const Polygon:TGeoPolygon2D):Boolean;
begin
  (*
    Simple polygon definition
    A polygon that can have:
    1.) inner and outter verticies
    2.) closed area
    3.) no self intersecting edges
  *)
  Result := not SelfIntersectingPolygon(Polygon);
end;
(* End of SimplePolygon *)


function ConvexPolygon(const Polygon:TGeoPolygon2D):Boolean;
var
  i                  : Integer;
  j                  : Integer;
  k                  : Integer;
  InitialOrientation : Integer;
  CurrentOrientation : Integer;
  FirstTime          : Boolean;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  FirstTime := True;
  InitialOrientation  := Orientation(Polygon[Length(Polygon) - 2],Polygon[Length(Polygon) - 1],Polygon[0].x,Polygon[0].y);
  j                   := 0;
  k                   := Length(Polygon) - 1;
  for i := 1 to Length(Polygon) - 1 do
  begin
    CurrentOrientation := Orientation(Polygon[k],Polygon[j],Polygon[i].x,Polygon[i].y);
    if (InitialOrientation = geoCollinearOrientation) and (InitialOrientation <> CurrentOrientation) and FirstTime then
    begin
      InitialOrientation := CurrentOrientation;
      FirstTime          := False;
    end
    else if (InitialOrientation <> CurrentOrientation) and (CurrentOrientation <> geoCollinearOrientation) then
      Exit;
    k := j;
    j := i;
  end;
  Result:= True;
end;
(* End of ConvexPolygon *)


function ConvexPolygon(const Polygon:array of TGeoPoint2D):Boolean;
var
  i                  : Integer;
  j                  : Integer;
  k                  : Integer;
  InitialOrientation : Integer;
  CurrentOrientation : Integer;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  InitialOrientation  := Orientation(Polygon[Length(Polygon) - 2],Polygon[Length(Polygon) - 1],Polygon[0].x,Polygon[0].y);
  j                   := 0;
  k                   := Length(Polygon) - 1;
  for i := 1 to Length(Polygon) - 1 do
  begin
    CurrentOrientation := Orientation(Polygon[k],Polygon[j],Polygon[i].x,Polygon[i].y);
    if (InitialOrientation = geoCollinearOrientation) and (InitialOrientation <> CurrentOrientation) then
      InitialOrientation := CurrentOrientation
    else if (InitialOrientation <> CurrentOrientation) and (CurrentOrientation <> geoCollinearOrientation) then
      Exit;
    k := j;
    j := i;
  end;
  Result:= True;
end;
(* End of ConvexPolygon *)


function ConcavePolygon(const Polygon:TGeoPolygon2D):Boolean;
begin
  Result := not ConvexPolygon(Polygon);
end;
(* End of ConcavePolygon *)


function ConvexPolygonOrientation(const Polygon:TGeoPolygon2D):Integer;
begin
  if Length(Polygon) < 3 then
  Result := 0
  else
  Result := Orientation(Polygon[0],Polygon[1],Polygon[2]);
end;
(* End of ConvexPolygonOrientation *)


function SimplePolygonOrientation(const Polygon:TGeoPolygon2D):Integer;
var
  I       : Integer;
  anchor  : Integer;
  prevpos : Integer;
  postpos : Integer;
begin
  Result := 0;
  if Length(Polygon) < 3 then Exit;
    anchor := 0;
  for i := 1 to Length(Polygon) - 1 do
  begin
    if Polygon[i].x > Polygon[anchor].x then
      anchor := i
    else if (Polygon[i].x = Polygon[anchor].x) and (Polygon[i].y  < Polygon[anchor].y) then
      anchor := i;
  end;
  postpos := (anchor + 1) mod Length(Polygon);
  prevpos := anchor - 1;
  if prevpos < 0 then
    prevpos := Length(Polygon) - prevpos;
  Result := Orientation(Polygon[prevpos],Polygon[postpos],Polygon[anchor]);
end;
(* End of SimplePolygonOrientation *)


function SelfIntersectingPolygon(const Polygon:TGeoPolygon2D):Boolean;
var
  I            : Integer;
  J            : Integer;
  Poly1Trailer : Integer;
  Poly2Trailer : Integer;
begin
  Result := False;
  if (Length(Polygon) < 3) or (Length(Polygon) < 3) then exit;
  Poly1Trailer := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    Poly2Trailer := i + 1;
    for j := i + 2 to Length(Polygon) - 2 do
    begin
      if (i <> j) and (Poly1Trailer <> Poly2Trailer) then
      begin
        if Intersect(Polygon[i],Polygon[Poly1Trailer],Polygon[j],Polygon[Poly2Trailer]) then
        begin
          Result := True;
          Exit;
        end;
      end;
      Poly2Trailer := j;
    end;
    Poly1Trailer := i;
  end;
end;
(* End of SelfIntersectingPolygon *)


function RectangularHull(const Point:array of TGeoPoint2D):TGeoRectangle;
var
  MaxX : TGeoFloat;
  MaxY : TGeoFloat;
  MinX : TGeoFloat;
  MinY : TGeoFloat;
  I    : Integer;
begin
  if Length(Point) < 2 then Exit;
  MinX := Point[0].x;
  MaxX := Point[0].x;
  MinY := Point[0].y;
  MaxY := Point[0].y;
  for i := 1 to Length(Point) - 1 do
  begin
    if Point[i].x < MinX then
      MinX := Point[i].x
    else if Point[i].x > MaxX then
      MaxX := Point[i].x;

    if Point[i].y < MinY then
      MinY := Point[i].y
    else if Point[i].y > MaxY then
      MaxY := Point[i].y;
  end;
  Result := EquateRectangle(MinX,MinY,MaxX,MaxY);
end;
(* End of RectangularHull *)


function RectangularHull(const Polygon:TGeoPolygon2D):TGeoRectangle;
var
  MaxX : TGeoFloat;
  MaxY : TGeoFloat;
  MinX : TGeoFloat;
  MinY : TGeoFloat;
  i    : Integer;
begin
  if Length(Polygon) < 2 then Exit;
  MinX := Polygon[0].x;
  MaxX := Polygon[0].x;
  MinY := Polygon[0].y;
  MaxY := Polygon[0].y;
  for i := 1 to Length(Polygon) - 1 do
  begin
    if Polygon[i].x < MinX then
      MinX := Polygon[i].x
    else if Polygon[i].x > MaxX then
      MaxX := Polygon[i].x;

    if Polygon[i].y < MinY then
      MinY := Polygon[i].y
    else if Polygon[i].y > MaxY then
      MaxY := Polygon[i].y;
  end;
  Result := EquateRectangle(MinX,MinY,MaxX,MaxY);
end;
(* End of RectangularHull *)


function CircularHull(const Polygon:TGeoPolygon2D):TGeoCircle;
var
  i       : Integer;
  Cen     : TGeoPoint2D;
  LayLen  : TGeoFloat;
  LayDist : TGeoFloat;
begin
  if Length(Polygon) < 3 then Exit;
    LayLen := -1;
  Cen := Centroid(Polygon);
  for i := 0 to Length(Polygon) - 1 do
  begin
    LayDist:= LayDistance(Cen,Polygon[i]);
    if LayDist > LayLen then
      LayLen := LayDist;
  end;
  Result.x      := Cen.x;
  Result.y      := Cen.y;
  Result.Radius := sqrt(LayLen);
end;
(* End of CircularHull *)


function SphereHull(const Polygon:array of TGeoPoint3D):TGeoSphere;
var
  i       : Integer;
  Cen     : TGeoPoint3D;
  LayLen  : TGeoFloat;
  LayDist : TGeoFloat;
begin
  if Length(Polygon) < 2 then Exit;
  LayLen := -1;
  Cen := Centroid(Polygon);
  for i := 0 to Length(Polygon) - 1 do
  begin
    LayDist := LayDistance(Cen,Polygon[i]);
    if LayDist > LayLen then
      LayLen := LayDist;
  end;
  Result.x      := Cen.x;
  Result.y      := Cen.y;
  Result.z      := Cen.z;
  Result.Radius := sqrt(LayLen);
end;
(* End of SphereHull *)


function CalculateBarycentricBase(const x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoFloat;
begin
 Result := Signed(x1,y1,x2,y2,x3,y3);
end;
(* End Of Calculate Barycentric Unit *)


function CreateBarycentricUnit(const x1,y1,x2,y2,x3,y3:TGeoFloat):TBarycentricUnit;
begin
  Result.x1    := x1;
  Result.y1    := y1;
  Result.x2    := x2;
  Result.y2    := y2;
  Result.x3    := x3;
  Result.y3    := y3;
  Result.delta := CalculateBarycentricBase(x1,y1,x2,y2,x3,y3);
end;
(* End of Create Barycentric Unit *)


function CreateBarycentricUnit(const Triangle : TGeoTriangle2D):TBarycentricUnit;
begin
  Result := CreateBarycentricUnit(Triangle[1].x,Triangle[1].y,
                                  Triangle[2].x,Triangle[2].y,
                                  Triangle[3].x,Triangle[3].y);
end;
(* End of Create Barycentric Unit *)


procedure ConvertCartesianToBarycentric(const x1,y1,x2,y2,x3,y3,px,py:TGeoFloat; out u,v,w: TGeoFloat);
var
  BarycentricBase : TGeoFloat;
begin
  BarycentricBase := 1 / CalculateBarycentricBase(x1,y1,x2,y2,x3,y3);
  u := CalculateBarycentricBase(Px,Py,x2,y2,x3,y3) * BarycentricBase;
  v := CalculateBarycentricBase(x1,y1,Px,Py,x3,y3) * BarycentricBase;
  w := CalculateBarycentricBase(x1,y1,x2,y2,Px,Py) * BarycentricBase;
end;
(* End Of Convert Cartesian to Barycentric *)


procedure ConvertCartesianToBarycentric(const BU:TBarycentricUnit; Px,Py:TGeoFloat; out u,v,w: TGeoFloat);
var
  BarycentricBase : TGeoFloat;
begin
  BarycentricBase := 1 / BU.delta;
  u := CalculateBarycentricBase(Px,Py,BU.x2,BU.y2,BU.x3,BU.y3) * BarycentricBase;
  v := CalculateBarycentricBase(BU.x1,BU.y1,Px,Py,BU.x3,BU.y3) * BarycentricBase;
  w := CalculateBarycentricBase(BU.x1,BU.y1,BU.x2,BU.y2,Px,Py) * BarycentricBase;
end;
(* End Of Convert Cartesian to Barycentric *)


procedure ConvertCartesianToBarycentric(const BU:TBarycentricUnit; const Point:TGeoPoint2D; out BCrd: TBarycentricTriplet);
begin
  ConvertCartesianToBarycentric(BU,Point.x,Point.y,BCrd.u,BCrd.v,BCrd.w);
end;
(* End Of Convert Cartesian to Barycentric *)


function ConvertCartesianToBarycentric(const BU:TBarycentricUnit; const Point:TGeoPoint2D): TBarycentricTriplet;
begin
  ConvertCartesianToBarycentric(BU,Point,Result);
end;
(* End Of Convert Cartesian to Barycentric *)


procedure ConvertBarycentricToCartesian(const u,v,w,x1,y1,x2,y2,x3,y3:TGeoFloat; out x,y:TGeoFloat);
begin
  x := u * x1 + v * x2 + w * x3;
  y := u * y1 + v * y2 + w * y3;
end;
(* End Of Convert Barycentric to Cartesian *)


procedure ConvertBarycentricToCartesian(const u,v,w:TGeoFloat; BU:TBarycentricUnit; out x,y:TGeoFloat);
begin
  ConvertBarycentricToCartesian(u,v,w,BU.x1,BU.y1,BU.x2,BU.y2,BU.x3,BU.y3,x,y);
end;
(* End Of Convert Barycentric to Cartesian *)


procedure ConvertBarycentricToCartesian(const u,v,w:TGeoFloat; BU:TBarycentricUnit; out Point:TGeoPoint2D);
begin
  ConvertBarycentricToCartesian(u,v,w,BU.x1,BU.y1,BU.x2,BU.y2,BU.x3,BU.y3,Point.x,Point.y);
end;
(* End Of Convert Barycentric to Cartesian *)


function ConvertBarycentricToCartesian(const u,v,w:TGeoFloat; BU:TBarycentricUnit):TGeoPoint2D;
begin
  ConvertBarycentricToCartesian(u,v,w,BU,Result);
end;
(* End Of Convert Barycentric to Cartesian *)


function Clip(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; out Cx1,Cy1,Cx2,Cy2 : TGeoFloat) : Boolean;
(* Rectangle(x1,y1,x2,y2) clipped against Rectangle(x3,y3,x4,y4) *)
begin
  if RectangleToRectangleIntersect(x1,y1,x2,y2,x3,y3,x4,y4) then
  begin
    Result := True;
    if x1 < x3 then Cx1 := x3 else Cx1 := x1;
    if x2 > x4 then Cx2 := x4 else Cx2 := x2;
    if y1 < y3 then Cy1 := y3 else Cy1 := y1;
    if y2 > y4 then Cy2 := y4 else Cy2 := y2;
  end
  else
    Result := False;
end;
(* End of Clip *)


function Clip(const Segment:TGeoSegment2D; const Rect:TGeoRectangle; out CSegment:TGeoSegment2D):Boolean;
 (* Cohen-Sutherland Clipping Algorithm *)

 const CLIP_BOTTOM = 1;
 const CLIP_TOP    = 2;
 const CLIP_LEFT   = 4;
 const CLIP_RIGHT  = 8;

  function OutCode(const x,y:TGeoFloat):Integer;
  begin
    Result := 0;

    if y < Rect[1].y then
      Result := Result or CLIP_TOP
    else if y > Rect[2].y then
      Result := Result or CLIP_BOTTOM;

    if x < Rect[1].x then
      Result := Result or CLIP_LEFT
    else if x > Rect[2].x then
      Result := Result or CLIP_RIGHT;
  end;

var
  outcode0   : Integer;
  outcode1   : Integer;
  outcodeout : Integer;
  x          : TGeoFloat;
  y          : TGeoFloat;
  Dx         : TGeoFloat;
  Dy         : TGeoFloat;
begin
  Result   := False;
  CSegment := Segment;
  x        := Zero;
  y        := Zero;

  OutCode0 := OutCode(CSegment[1].x,CSegment[1].y);
  OutCode1 := OutCode(CSegment[2].x,CSegment[2].y);

  while (OutCode0 <> 0) or (OutCode1 <> 0) do
  begin
    if (OutCode0 and OutCode1) <> 0 then
    Exit
    else
    begin
      if Outcode0 <> 0 then
        OutCodeout := OutCode0
      else
        OutCodeout := OutCode1;

      Dx := (CSegment[2].x - CSegment[1].x);
      Dy := (CSegment[2].y - CSegment[1].y);

      if ((OutCodeout and CLIP_TOP) = CLIP_TOP) then
      begin
        x := CSegment[1].x + Dx * (Rect[1].y - CSegment[1].y) / Dy;
        y := Rect[1].y;
      end
      else if ((OutCodeout and CLIP_BOTTOM) = CLIP_BOTTOM) then
      begin
        x := CSegment[1].x + Dx * (Rect[2].y - CSegment[1].y) / Dy;
        y := Rect[2].y;
      end
      else if ((OutCodeout and CLIP_RIGHT) = CLIP_RIGHT) then
      begin
        y := CSegment[1].y + Dy * (Rect[2].x - CSegment[1].x) / Dx;
        x := Rect[2].x;
      end
      else if ((OutCodeout and CLIP_LEFT) = CLIP_LEFT) then
      begin
        y := CSegment[1].y + Dy * (Rect[1].x - CSegment[1].x) / Dx;
        x := Rect[1].x;
      end;

      if (OutCodeout = outcode0) then
      begin
        CSegment[1].x := x;
        CSegment[1].y := y;
        OutCode0  := OutCode(CSegment[1].x,CSegment[1].y);
      end
      else
      begin
        CSegment[2].x := x;
        CSegment[2].y := y;
        OutCode1  := OutCode(CSegment[2].x,CSegment[2].y);
      end
    end;
  end;
  Result := true;
end;
(* End of Clip *)


function Clip(const Segment:TGeoSegment2D; const Triangle:TGeoTriangle2D; out CSegment:TGeoSegment2D):Boolean;
var
  Pos : Integer;
begin
  if not Intersect(Segment,Triangle) then
  begin
    Result := False;
    Exit;
  end;
  Pos := 1;
  CSegment := Segment;

  if Intersect(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if Intersect(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if (Pos < 3) then
  if Intersect(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Triangle[3].x,Triangle[3].y,Triangle[1].x,Triangle[1].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if Pos = 2 then
  begin
    if PointInTriangle(Segment[1],Triangle) then
      CSegment[Pos] := Segment[1]
    else
      CSegment[Pos] := Segment[2];
  end;
  Result := True;
end;
(* End of Clip *)


function Clip(const Segment:TGeoSegment2D; const Quadix:TQuadix2D; out CSegment:TGeoSegment2D):Boolean;
var
  Pos : Integer;
begin
  if not Intersect(Segment,Quadix) then
  begin
    Result := False;
    Exit;
  end;
  Pos := 1;
  CSegment := Segment;

  if Intersect(Segment[1].x,Segment[1].y, Segment[2].x,Segment[2].y,Quadix[1].x,Quadix[1].y,Quadix[2].x,Quadix[2].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if Intersect(Segment[1].x,Segment[1].y, Segment[2].x,Segment[2].y,Quadix[2].x,Quadix[2].y,Quadix[3].x,Quadix[3].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if (Pos < 3) then
  if Intersect(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Quadix[3].x,Quadix[3].y,Quadix[4].x,Quadix[4].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if (Pos < 3) then
  if Intersect(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Quadix[4].x,Quadix[4].y,Quadix[1].x,Quadix[1].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if Pos = 2 then
  begin
    if PointInQuadix(Segment[1],Quadix) then
    CSegment[Pos]:= Segment[1]
    else
    CSegment[Pos]:= Segment[2];
  end;
  Result := True;
end;
(* End of Clip *)


function Clip(const Segment:TGeoSegment2D; const Circle:TGeoCircle; out CSegment:TGeoSegment2D):Boolean;
var
  Cnt : Integer;
  I1  : TGeoPoint2D;
  I2  : TGeoPoint2D;
begin
  Result := False;
  IntersectionPoint(Segment,Circle,Cnt,I1,I2);
  if Cnt = 2 then
  begin
    CSegment[1] := I1;
    CSegment[2] := I2;
    Result      := True;
  end
end;
(* End of Clip *)


function Clip(const Segment:TGeoSegment2D; const Obj : TGeometricObject; out CSegment:TGeoSegment2D):Boolean;
begin
  Result := False;
  case Obj.ObjectType of
    geoRectangle  : Result := Clip(Segment,Obj.Rectangle ,CSegment);
    geoTriangle2D : Result := Clip(Segment,Obj.Triangle2D,CSegment);
    geoQuadix2D   : Result := Clip(Segment,Obj.Quadix2D  ,CSegment);
    geoCircle     : Result := Clip(Segment,Obj.Circle    ,CSegment);
  end;
end;
(* End of Clip *)


function Clip(const Rect1,Rect2 : TGeoRectangle; out CRect:TGeoRectangle):Boolean;
(* Rect1 clipped against Rect2 *)
begin
  Result := Clip(Rect1[1].x, Rect1[1].y, Rect1[2].x, Rect1[2].y,
                 Rect2[1].x, Rect2[1].y, Rect2[2].x, Rect2[2].y,
                 CRect[1].x, CRect[1].y, CRect[2].x, CRect[2].y);
end;
(* End of Clip *)


function Area(const Point1,Point2,Point3:TGeoPoint2D):TGeoFloat;
begin
  Result := 0.5 *
                  (
                    (Point1.x * (Point2.y - Point3.y)) +
                    (Point2.x * (Point3.y - Point1.y)) +
                    (Point3.x * (Point1.y - Point2.y))
                  );
end;
(* End of Area 3-2D Points *)


function Area(const Point1,Point2,Point3:TGeoPoint3D):TGeoFloat;
var
  Dx1 : TGeoFloat;
  Dx2 : TGeoFloat;
  Dy1 : TGeoFloat;
  Dy2 : TGeoFloat;
  Dz1 : TGeoFloat;
  Dz2 : TGeoFloat;
  Cx  : TGeoFloat;
  Cy  : TGeoFloat;
  Cz  : TGeoFloat;
begin
  Dx1 := Point2.x - Point1.x;
  Dy1 := Point2.y - Point1.y;
  Dz1 := Point2.z - Point1.z;

  Dx2 := Point3.x - Point1.x;
  Dy2 := Point3.y - Point1.y;
  Dz2 := Point3.z - Point1.z;

  Cx  := Dy1 * Dz2 - Dy2 * Dz1;
  Cy  := Dx2 * Dz1 - Dx1 * Dz2;
  Cz  := Dx1 * Dy2 - Dx2 * Dy1;

  Result := (sqrt(Cx * Cx + Cy * Cy + Cz * Cz) * 0.5);
end;
(* End of Area 3-3D Points *)


function Area(const Triangle:TGeoTriangle2D):TGeoFloat;
begin
  Result := 0.5 *
                 (
                   (Triangle[1].x * (Triangle[2].y - Triangle[3].y)) +
                   (Triangle[2].x * (Triangle[3].y - Triangle[1].y)) +
                   (Triangle[3].x * (Triangle[1].y - Triangle[2].y))
                 );
end;
(* End of Area 2D Triangle *)


function Area(const Triangle:TGeoTriangle3D):TGeoFloat;
var
  Dx1 : TGeoFloat;
  Dx2 : TGeoFloat;
  Dy1 : TGeoFloat;
  Dy2 : TGeoFloat;
  Dz1 : TGeoFloat;
  Dz2 : TGeoFloat;
  Cx  : TGeoFloat;
  Cy  : TGeoFloat;
  Cz  : TGeoFloat;
begin
  Dx1 := Triangle[2].x - Triangle[1].x;
  Dy1 := Triangle[2].y - Triangle[1].y;
  Dz1 := Triangle[2].z - Triangle[1].z;

  Dx2 := Triangle[3].x - Triangle[1].x;
  Dy2 := Triangle[3].y - Triangle[1].y;
  Dz2 := Triangle[3].z - Triangle[1].z;

  Cx  := Dy1 * Dz2 - Dy2 * Dz1;
  Cy  := Dx2 * Dz1 - Dx1 * Dz2;
  Cz  := Dx1 * Dy2 - Dx2 * Dy1;

  Result := (sqrt(Cx * Cx + Cy * Cy + Cz * Cz) * 0.5);
end;
(* End of Area 3D Triangle *)


function Area(const Quadix:TQuadix2D):TGeoFloat;
begin
  Result := 0.5 *
                 (
                  (Quadix[1].x * (Quadix[2].y - Quadix[4].y)) +
                  (Quadix[2].x * (Quadix[3].y - Quadix[1].y)) +
                  (Quadix[3].x * (Quadix[4].y - Quadix[2].y)) +
                  (Quadix[4].x * (Quadix[1].y - Quadix[3].y))
                 );
end;
(* End of Area 2D Qudix *)


function Area(const Quadix:TQuadix3D):TGeoFloat;
begin
  Result := (
             Area(EquateTriangle(Quadix[1],Quadix[2],Quadix[3])) +
             Area(EquateTriangle(Quadix[3],Quadix[4],Quadix[1]))
            );
end;
(* End of Area 3D Quadix *)


function Area(const Rectangle:TGeoRectangle):TGeoFloat;
begin
  Result := abs(Rectangle[2].x - Rectangle[1].x) * abs(Rectangle[2].y - Rectangle[1].y);
end;
(* End of Area *)


function Area(const Circle:TGeoCircle):TGeoFloat;
begin
  Result := PI * Circle.Radius * Circle.Radius;
end;
(* End of Area *)


function Area(const Polygon:TGeoPolygon2D):TGeoFloat;
var
  i : Integer;
  j : Integer;
begin
  Result := Zero;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result := Result + ((Polygon[j].x * Polygon[i].y) - (Polygon[j].y * Polygon[i].x));
    j := i;
  end;
  Result := Result * 0.5;
end;
(* End of Area *)


function Area(const Obj:TGeometricObject):TGeoFloat;
begin
  case Obj.ObjectType of
    geoTriangle2D: Result := Area(Obj.Triangle2D);
    geoTriangle3D: Result := Area(Obj.Triangle3D);
    geoQuadix2D  : Result := Area(Obj.Quadix2D  );
    geoQuadix3D  : Result := Area(Obj.Quadix3D  );
    geoRectangle : Result := Area(Obj.Rectangle );
    geoCircle    : Result := Area(Obj.Circle    );
  else
    Result := Zero;
  end;
end;
(* End of Area *)


function Perimeter(const Triangle:TGeoTriangle2D):TGeoFloat;
begin
  Result:= Distance(Triangle[1],Triangle[2]) +
           Distance(Triangle[2],Triangle[3]) +
           Distance(Triangle[3],Triangle[1]);
end;
(* End of Perimeter *)


function Perimeter(const Triangle:TGeoTriangle3D):TGeoFloat;
begin
  Result:= Distance(Triangle[1],Triangle[2]) +
           Distance(Triangle[2],Triangle[3]) +
           Distance(Triangle[3],Triangle[1]);
end;
(* End of Perimeter *)
function Perimeter(const Quadix:TQuadix2D):TGeoFloat;
begin
  Result:= Distance(Quadix[1],Quadix[2]) +
           Distance(Quadix[2],Quadix[3]) +
           Distance(Quadix[3],Quadix[4]) +
           Distance(Quadix[4],Quadix[1]);
end;
(* End of Perimeter *)


function Perimeter(const Quadix:TQuadix3D):TGeoFloat;
begin
  Result:= Distance(Quadix[1],Quadix[2]) +
           Distance(Quadix[2],Quadix[3]) +
           Distance(Quadix[3],Quadix[4]) +
           Distance(Quadix[4],Quadix[1]);
end;
(* End of Perimeter *)


function Perimeter(const Rectangle:TGeoRectangle):TGeoFloat;
begin
  Result:= 2.0 * (abs(Rectangle[2].x - Rectangle[1].x) + abs(Rectangle[2].y - Rectangle[1].y));
end;
(* End of Perimeter *)


function Perimeter(const Circle:TGeoCircle):TGeoFloat;
begin
  Result:= 2.0 * Pi * Circle.Radius;
end;
(* End of Perimeter *)


function Perimeter(const Polygon:TGeoPolygon2D):TGeoFloat;
var
  i : Integer;
  j : Integer;
begin
  Result := Zero;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result := Result + Distance(Polygon[i], Polygon[j]);
    j := i;
  end;
end;
(* End of Perimeter *)


function Perimeter(const Obj:TGeometricObject):TGeoFloat;
begin
  case Obj.ObjectType of
    geoTriangle2D : Result := Perimeter(Obj.Triangle2D);
    geoTriangle3D : Result := Perimeter(Obj.Triangle3D);
    geoQuadix2D   : Result := Perimeter(Obj.Quadix2D  );
    geoQuadix3D   : Result := Perimeter(Obj.Quadix3D  );
    geoRectangle  : Result := Perimeter(Obj.Rectangle );
    geoCircle     : Result := Perimeter(Obj.Circle    );
  else
    Result := Zero;
  end;
end;
(* End of Perimeter *)


function SemiPerimeter(const Triangle : TGeoTriangle2D):TGeoFloat;
begin
  Result := Perimeter(Triangle) * 0.5;
end;
(* End of Perimeter *)


function SemiPerimeter(const Triangle : TGeoTriangle3D):TGeoFloat;
begin
  Result := Perimeter(Triangle) * 0.5;
end;
(* End of Perimeter *)


procedure Rotate(RotAng:TGeoFloat; const x,y:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  SinVal : TGeoFloat;
  CosVal : TGeoFloat;
begin
  RotAng := RotAng * PIDiv180;
  sincos(RotAng, SinVal, CosVal);
  //SinVal := Sin(RotAng);
  //CosVal := Cos(RotAng);
  Nx     := (x * CosVal) - (y * SinVal);
  Ny     := (y * CosVal) + (x * SinVal);
end;
(* End of Rotate Cartesian Point *)


procedure Rotate(const RotAng:TGeoFloat; const x,y,ox,oy:TGeoFloat; out Nx,Ny:TGeoFloat);
begin
  Rotate(RotAng,x - ox,y - oy,Nx,Ny);
  Nx := Nx + ox;
  Ny := Ny + oy;
end;
(* End of Rotate Cartesian Point About Origin *)


function Rotate(const RotAng:TGeoFloat; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  Rotate(RotAng,Point.x,Point.y,Result.x,Result.y);
end;
(* End of Rotate Point *)


function Rotate(const RotAng:TGeoFloat; const Point,OPoint:TGeoPoint2D):TGeoPoint2D;
begin
  Rotate(RotAng,Point.x,Point.y,OPoint.x,OPoint.y,Result.x,Result.y);
end;
(* End of Rotate Point About Origin *)


function Rotate(const RotAng:TGeoFloat; const Segment:TGeoSegment2D):TGeoSegment2D;
begin
  Result[1] := Rotate(RotAng,Segment[1]);
  Result[2] := Rotate(RotAng,Segment[2]);
end;
(* End of Rotate Segment*)


function Rotate(const RotAng:TGeoFloat; const Segment:TGeoSegment2D; const OPoint:TGeoPoint2D):TGeoSegment2D;
begin
  Result[1] := Rotate(RotAng,Segment[1],OPoint);
  Result[2] := Rotate(RotAng,Segment[2],OPoint);
end;
(* End of Rotate Segment About Origin *)


function Rotate(const RotAng:TGeoFloat; const Triangle:TGeoTriangle2D):TGeoTriangle2D;
begin
  Result[1] := Rotate(RotAng,Triangle[1]);
  Result[2] := Rotate(RotAng,Triangle[2]);
  Result[3] := Rotate(RotAng,Triangle[3]);
end;
(* End of Rotate 2D Triangle*)


function Rotate(const RotAng:TGeoFloat; const Triangle:TGeoTriangle2D; const OPoint:TGeoPoint2D):TGeoTriangle2D;
begin
  Result[1] := Rotate(RotAng,Triangle[1],OPoint);
  Result[2] := Rotate(RotAng,Triangle[2],OPoint);
  Result[3] := Rotate(RotAng,Triangle[3],OPoint);
end;
(* End of Rotate 2D Triangle About Origin *)


function Rotate(const RotAng:TGeoFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1] := Rotate(RotAng,Quadix[1]);
  Result[2] := Rotate(RotAng,Quadix[2]);
  Result[3] := Rotate(RotAng,Quadix[3]);
  Result[4] := Rotate(RotAng,Quadix[4]);
end;
(* End of Rotate 2D Quadix *)


function Rotate(const RotAng:TGeoFloat; const Quadix:TQuadix2D; const OPoint:TGeoPoint2D):TQuadix2D;
begin
  Result[1] := Rotate(RotAng,Quadix[1],OPoint);
  Result[2] := Rotate(RotAng,Quadix[2],OPoint);
  Result[3] := Rotate(RotAng,Quadix[3],OPoint);
  Result[4] := Rotate(RotAng,Quadix[4],OPoint);
end;
(* End of Rotate 2D Quadix About Origin *)


function Rotate(const RotAng:TGeoFloat; const Polygon:TGeoPolygon2D):TGeoPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(RotAng,Polygon[i]);
  end;
end;
(* End of Rotate 2D Polygon *)


function Rotate(const RotAng:TGeoFloat; const Polygon:TGeoPolygon2D; const OPoint:TGeoPoint2D):TGeoPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(RotAng,Polygon[i],OPoint);
  end;
end;
(* End of Rotate 2D Polygon About Origin *)


function Rotate(const RotAng:TGeoFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := Rotate(RotAng,Obj.Point2D);
    geoSegment2D : Result.Segment2D  := Rotate(RotAng,Obj.Segment2D);
    geoTriangle2D: Result.Triangle2D := Rotate(RotAng,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := Rotate(RotAng,Obj.Quadix2D);
  else
    Result := Obj;
  end;
end;
(* End of Rotate Geometric Object *)


function Rotate(const RotAng:TGeoFloat; const Obj:TGeometricObject; const OPoint: TGeoPoint2D):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := Rotate(RotAng,Obj.Point2D,OPoint);
    geoSegment2D : Result.Segment2D  := Rotate(RotAng,Obj.Segment2D,OPoint);
    geoTriangle2D: Result.Triangle2D := Rotate(RotAng,Obj.Triangle2D,OPoint);
    geoQuadix2D  : Result.Quadix2D   := Rotate(RotAng,Obj.Quadix2D,OPoint);
  else
    Result := Obj;
  end;
end;
(* End of Rotate Geometric Object About Origin *)


procedure Rotate(const Rx,Ry,Rz:TGeoFloat; const x,y,z:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);
var
  TempX   : TGeoFloat;
  TempY   : TGeoFloat;
  TempZ   : TGeoFloat;
  SinX    : TGeoFloat;
  SinY    : TGeoFloat;
  SinZ    : TGeoFloat;
  CosX    : TGeoFloat;
  CosY    : TGeoFloat;
  CosZ    : TGeoFloat;
  XRadAng : TGeoFloat;
  YRadAng : TGeoFloat;
  ZRadAng : TGeoFloat;
begin
  XRadAng := Rx * PIDiv180;
  YRadAng := Ry * PIDiv180;
  ZRadAng := Rz * PIDiv180;

  Sincos(XRadAng, SinX, CosX);
  Sincos(YRadAng, SinY, CosY);
  Sincos(ZRadAng, SinZ, CosZ);

  Tempy   := y * CosY -     z * SinY;
  Tempz   := y * SinY +     z * CosY;
  Tempx   := x * CosX - Tempz * SinX;

  Nz      :=     x * SinX + Tempz * CosX;
  Nx      := Tempx * CosZ - TempY * SinZ;
  Ny      := Tempx * SinZ + TempY * CosZ;
end;
(* End of Rotate 3D Point *)


procedure Rotate(const Rx,Ry,Rz:TGeoFloat; const x,y,z,ox,oy,oz:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);
begin
  Rotate(Rx,Ry,Rz,x - ox,y - oy,z - oz,Nx,Ny,Nz);
  Nx := Nx + ox;
  Ny := Ny + oy;
  Nz := Nz + oz;
end;
(* End of Rotate 3D Point About Origin Point *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Point:TGeoPoint3D):TGeoPoint3D;
begin
  Rotate(Rx,Ry,Rz,Point.x,Point.y,Point.z,Result.x,Result.y,Result.z);
end;
(* End of Rotate 3D Point *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Point,OPoint:TGeoPoint3D):TGeoPoint3D;
begin
  Rotate(Rx,Ry,Rz,Point.x,Point.y,Point.z,OPoint.x,OPoint.y,OPoint.z,Result.x,Result.y,Result.z);
end;
(* End of Rotate 3D Point About Origin Point *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Segment:TGeoSegment3D):TGeoSegment3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Segment[1]);
  Result[2] := Rotate(Rx,Ry,Rz,Segment[2]);
end;
(* End of Rotate 3D Segment *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Segment:TGeoSegment3D; const OPoint:TGeoPoint3D):TGeoSegment3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Segment[1],OPoint);
  Result[2] := Rotate(Rx,Ry,Rz,Segment[2],OPoint);
end;
(* End of Rotate 3D Segment About Origin Point *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Triangle:TGeoTriangle3D):TGeoTriangle3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Triangle[1]);
  Result[2] := Rotate(Rx,Ry,Rz,Triangle[2]);
  Result[3] := Rotate(Rx,Ry,Rz,Triangle[3]);
end;
(* End of Rotate 3D Triangle *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Triangle:TGeoTriangle3D; const OPoint:TGeoPoint3D):TGeoTriangle3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Triangle[1],OPoint);
  Result[2] := Rotate(Rx,Ry,Rz,Triangle[2],OPoint);
  Result[3] := Rotate(Rx,Ry,Rz,Triangle[3],OPoint);
end;
(* End of Rotate 3D Triangle About Origin Point *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Quadix:TQuadix3D):TQuadix3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Quadix[1]);
  Result[2] := Rotate(Rx,Ry,Rz,Quadix[2]);
  Result[3] := Rotate(Rx,Ry,Rz,Quadix[3]);
  Result[4] := Rotate(Rx,Ry,Rz,Quadix[4]);
end;
(* End of Rotate 3D Quadix *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Quadix:TQuadix3D; const OPoint:TGeoPoint3D):TQuadix3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Quadix[1],OPoint);
  Result[2] := Rotate(Rx,Ry,Rz,Quadix[2],OPoint);
  Result[3] := Rotate(Rx,Ry,Rz,Quadix[3],OPoint);
  Result[4] := Rotate(Rx,Ry,Rz,Quadix[4],OPoint);
end;
(* End of Rotate 3D Quadix About Origin Point *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Polygon:TGeoPolygon3D):TGeoPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(Rx,Ry,Rz,Polygon[i]);
  end;
end;
(* End of Rotate 3D Polygon *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Polygon:TGeoPolygon3D; const OPoint:TGeoPoint3D):TGeoPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(Rx,Ry,Rz,Polygon[i],OPoint);
  end;
end;
(* End of Rotate 3D Polygon About Origin Point *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint3D   : Result.Point3D    := Rotate(Rx,Ry,Rz,Obj.Point3D   );
    geoSegment3D : Result.Segment3D  := Rotate(Rx,Ry,Rz,Obj.Segment3D );
    geoTriangle3D: Result.Triangle3D := Rotate(Rx,Ry,Rz,Obj.Triangle3D);
    geoQuadix3D  : Result.Quadix3D   := Rotate(Rx,Ry,Rz,Obj.Quadix3D  );
  else
    Result := Obj;
  end;
end;
(* End of Rotate *)


function Rotate(const Rx,Ry,Rz:TGeoFloat; const Obj:TGeometricObject; const OPoint: TGeoPoint3D):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint3D   : Result.Point3D    := Rotate(Rx,Ry,Rz,Obj.Point3D,OPoint   );
    geoSegment3D : Result.Segment3D  := Rotate(Rx,Ry,Rz,Obj.Segment3D,OPoint );
    geoTriangle3D: Result.Triangle3D := Rotate(Rx,Ry,Rz,Obj.Triangle3D,OPoint);
    geoQuadix3D  : Result.Quadix3D   := Rotate(Rx,Ry,Rz,Obj.Quadix3D,OPoint  );
  else
    Result := Obj;
  end;
end;
(* End of Rotate About Origin Point *)


procedure FastRotate(RotAng:Integer; const x,y:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  SinVal : TGeoFloat;
  CosVal : TGeoFloat;
begin
  RotAng := RotAng mod 360;
  if RotAng < 0 then RotAng := 360 + RotAng;
  SinVal := SinTable[RotAng];
  CosVal := CosTable[RotAng];
  Nx     := x * CosVal - y * SinVal;
  Ny     := y * CosVal + x * SinVal;
end;
(* End of Fast Rotation *)


procedure FastRotate(RotAng:Integer; x,y,ox,oy:TGeoFloat; out Nx,Ny:TGeoFloat);
begin
  FastRotate(RotAng,x - ox,y - oy,Nx,Ny);
  Nx := Nx + ox;
  Ny := Ny + oy;
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  FastRotate(RotAng,Point.x,Point.y,Result.x,Result.y);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Point,OPoint:TGeoPoint2D):TGeoPoint2D;
begin
  FastRotate(RotAng,Point.x,Point.y,OPoint.x,OPoint.y,Result.x,Result.y);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Segment:TGeoSegment2D):TGeoSegment2D;
begin
  Result[1] := FastRotate(RotAng,Segment[1]);
  Result[2] := FastRotate(RotAng,Segment[2]);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Segment:TGeoSegment2D; const OPoint:TGeoPoint2D):TGeoSegment2D;
begin
  Result[1] := FastRotate(RotAng,Segment[1],OPoint);
  Result[2] := FastRotate(RotAng,Segment[2],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Triangle:TGeoTriangle2D):TGeoTriangle2D;
begin
  Result[1] := FastRotate(RotAng,Triangle[1]);
  Result[2] := FastRotate(RotAng,Triangle[2]);
  Result[3] := FastRotate(RotAng,Triangle[3]);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Triangle:TGeoTriangle2D; const OPoint:TGeoPoint2D):TGeoTriangle2D;
begin
  Result[1] := FastRotate(RotAng,Triangle[1],OPoint);
  Result[2] := FastRotate(RotAng,Triangle[2],OPoint);
  Result[3] := FastRotate(RotAng,Triangle[3],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1] := FastRotate(RotAng,Quadix[1]);
  Result[2] := FastRotate(RotAng,Quadix[2]);
  Result[3] := FastRotate(RotAng,Quadix[3]);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Quadix:TQuadix2D; const OPoint:TGeoPoint2D):TQuadix2D;
begin
  Result[1] := FastRotate(RotAng,Quadix[1],OPoint);
  Result[2] := FastRotate(RotAng,Quadix[2],OPoint);
  Result[3] := FastRotate(RotAng,Quadix[3],OPoint);
  Result[4] := FastRotate(RotAng,Quadix[4],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Polygon:TGeoPolygon2D):TGeoPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(RotAng,Polygon[i]);
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Polygon:TGeoPolygon2D; const OPoint:TGeoPoint2D):TGeoPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(RotAng,Polygon[i],OPoint);
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := FastRotate(RotAng,Obj.Point2D   );
    geoSegment2D : Result.Segment2D  := FastRotate(RotAng,Obj.Segment2D );
    geoTriangle2D: Result.Triangle2D := FastRotate(RotAng,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := FastRotate(RotAng,Obj.Quadix2D  );
  else
    Result := Obj;
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Obj:TGeometricObject; const OPoint: TGeoPoint2D):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := FastRotate(RotAng,Obj.Point2D,OPoint   );
    geoSegment2D : Result.Segment2D  := FastRotate(RotAng,Obj.Segment2D,OPoint );
    geoTriangle2D: Result.Triangle2D := FastRotate(RotAng,Obj.Triangle2D,OPoint);
    geoQuadix2D  : Result.Quadix2D   := FastRotate(RotAng,Obj.Quadix2D,OPoint  );
  else
    Result := Obj;
  end;
end;
(* End of Fast Rotation *)


procedure FastRotate(Rx,Ry,Rz:Integer; const x,y,z:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);
var
  TempX : TGeoFloat;
  TempY : TGeoFloat;
  TempZ : TGeoFloat;
  SinX  : TGeoFloat;
  SinY  : TGeoFloat;
  SinZ  : TGeoFloat;
  CosX  : TGeoFloat;
  CosY  : TGeoFloat;
  CosZ  : TGeoFloat;
begin
  Rx := Rx mod 360;
  Ry := Ry mod 360;
  Rz := Rz mod 360;

  if Rx < 0 then Rx := 360 + Rx;
  if Ry < 0 then Ry := 360 + Ry;
  if Rz < 0 then Rz := 360 + Rz;

  SinX := SinTable[Rx];
  SinY := SinTable[Ry];
  SinZ := SinTable[Rz];

  CosX := CosTable[Rx];
  CosY := CosTable[Ry];
  CosZ := CosTable[Rz];

  Tempy := y * CosY - z * SinY;
  Tempz := y * SinY + z * CosY;
  Tempx := x * CosX - Tempz * SinX;

  Nz    :=     x * SinX + Tempz * CosX;
  Nx    := Tempx * CosZ - TempY * SinZ;
  Ny    := Tempx * SinZ + TempY * CosZ;
end;
(* End of Fast Rotation *)


procedure FastRotate(const Rx,Ry,Rz:Integer; const x,y,z,ox,oy,oz:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);
begin
  FastRotate(Rx,Ry,Rz,x - ox,y - oy,z - oz,Nx,Ny,Nz);
  Nx := Nx + ox;
  Ny := Ny + oy;
  Nz := Nz + oz;
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Point:TGeoPoint3D):TGeoPoint3D;
begin
  FastRotate(Rx,Ry,Rz,Point.x,Point.y,Point.z,Result.x,Result.y,Result.z);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Point,OPoint:TGeoPoint3D):TGeoPoint3D;
begin
  FastRotate(Rx,Ry,Rz,Point.x,Point.y,Point.z,OPoint.x,OPoint.y,OPoint.z,Result.x,Result.y,Result.z);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Segment:TGeoSegment3D):TGeoSegment3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Segment[1]);
  Result[2] := FastRotate(Rx,Ry,Rz,Segment[2]);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Segment:TGeoSegment3D; const OPoint:TGeoPoint3D):TGeoSegment3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Segment[1],OPoint);
  Result[2] := FastRotate(Rx,Ry,Rz,Segment[2],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Triangle:TGeoTriangle3D):TGeoTriangle3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Triangle[1]);
  Result[2] := FastRotate(Rx,Ry,Rz,Triangle[2]);
  Result[3] := FastRotate(Rx,Ry,Rz,Triangle[3]);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Triangle:TGeoTriangle3D; const OPoint:TGeoPoint3D):TGeoTriangle3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Triangle[1],OPoint);
  Result[2] := FastRotate(Rx,Ry,Rz,Triangle[2],OPoint);
  Result[3] := FastRotate(Rx,Ry,Rz,Triangle[3],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Quadix:TQuadix3D):TQuadix3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Quadix[1]);
  Result[2] := FastRotate(Rx,Ry,Rz,Quadix[2]);
  Result[3] := FastRotate(Rx,Ry,Rz,Quadix[3]);
  Result[4] := FastRotate(Rx,Ry,Rz,Quadix[4]);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Quadix:TQuadix3D; const OPoint:TGeoPoint3D):TQuadix3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Quadix[1],OPoint);
  Result[2] := FastRotate(Rx,Ry,Rz,Quadix[2],OPoint);
  Result[3] := FastRotate(Rx,Ry,Rz,Quadix[3],OPoint);
  Result[4] := FastRotate(Rx,Ry,Rz,Quadix[4],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Polygon:TGeoPolygon3D):TGeoPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := FastRotate(Rx,Ry,Rz,Polygon[i]);
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Polygon:TGeoPolygon3D; const OPoint:TGeoPoint3D):TGeoPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := FastRotate(Rx,Ry,Rz,Polygon[i],OPoint);
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point3D    := FastRotate(Rx,Ry,Rz,Obj.Point3D   );
    geoSegment2D : Result.Segment3D  := FastRotate(Rx,Ry,Rz,Obj.Segment3D );
    geoTriangle2D: Result.Triangle3D := FastRotate(Rx,Ry,Rz,Obj.Triangle3D);
    geoQuadix2D  : Result.Quadix3D   := FastRotate(Rx,Ry,Rz,Obj.Quadix3D  );
  else
    Result := Obj;
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Obj:TGeometricObject; const OPoint: TGeoPoint3D):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point3D    := FastRotate(Rx,Ry,Rz,Obj.Point3D,OPoint   );
    geoSegment2D : Result.Segment3D  := FastRotate(Rx,Ry,Rz,Obj.Segment3D,OPoint );
    geoTriangle2D: Result.Triangle3D := FastRotate(Rx,Ry,Rz,Obj.Triangle3D,OPoint);
    geoQuadix2D  : Result.Quadix3D   := FastRotate(Rx,Ry,Rz,Obj.Quadix3D,OPoint  );
  else
    Result := Obj;
  end;
end;
(* End of Fast Rotation *)


function Translate(const Dx,Dy:TGeoFloat; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  Result.x := Point.x + Dx;
  Result.y := Point.y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TGeoFloat; const Line:TGeoLine2D):TGeoLine2D;
begin
  Result[1].x := Line[1].x + Dx;
  Result[1].y := Line[1].y + Dy;
  Result[2].x := Line[2].x + Dx;
  Result[2].y := Line[2].y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TGeoFloat; const Segment:TGeoSegment2D):TGeoSegment2D;
begin
  Result[1].x := Segment[1].x + Dx;
  Result[1].y := Segment[1].y + Dy;
  Result[2].x := Segment[2].x + Dx;
  Result[2].y := Segment[2].y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TGeoFloat; const Triangle:TGeoTriangle2D):TGeoTriangle2D;
begin
  Result[1].x := Triangle[1].x + Dx;
  Result[1].y := Triangle[1].y + Dy;
  Result[2].x := Triangle[2].x + Dx;
  Result[2].y := Triangle[2].y + Dy;
  Result[3].x := Triangle[3].x + Dx;
  Result[3].y := Triangle[3].y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TGeoFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1].x := Quadix[1].x + Dx;
  Result[1].y := Quadix[1].y + Dy;
  Result[2].x := Quadix[2].x + Dx;
  Result[2].y := Quadix[2].y + Dy;
  Result[3].x := Quadix[3].x + Dx;
  Result[3].y := Quadix[3].y + Dy;
  Result[4].x := Quadix[4].x + Dx;
  Result[4].y := Quadix[4].y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TGeoFloat; const Rectangle:TGeoRectangle):TGeoRectangle;
begin
  Result[1].x := Rectangle[1].x + Dx;
  Result[1].y := Rectangle[1].y + Dy;
  Result[2].x := Rectangle[2].x + Dx;
  Result[2].y := Rectangle[2].y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TGeoFloat; const Circle:TGeoCircle):TGeoCircle;
begin
  Result.x      := Circle.x + Dx;
  Result.y      := Circle.y + Dy;
  Result.Radius := Circle.Radius;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TGeoFloat; const Polygon:TGeoPolygon2D):TGeoPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i].x := Polygon[i].x + Dx;
    Result[i].y := Polygon[i].y + Dy;
  end;
end;
(* End of Translate *)


function Translate(const Point:TGeoPoint2D; const Polygon:TGeoPolygon2D):TGeoPolygon2D;
begin
  Result:= Translate(Point.x,Point.y,Polygon);
end;
(* End of Translate *)


function Translate(const Dx,Dy:TGeoFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := Translate(Dx,Dy,Obj.Point2D   );
    geoSegment2D : Result.Segment2D  := Translate(Dx,Dy,Obj.Segment2D );
    geoLine2D    : Result.Line2D     := Translate(Dx,Dy,Obj.Line2D    );
    geoRectangle : Result.Rectangle  := Translate(Dx,Dy,Obj.Rectangle );
    geoTriangle2D: Result.Triangle2D := Translate(Dx,Dy,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := Translate(Dx,Dy,Obj.Quadix2D  );
    geoCircle    : Result.Circle     := Translate(Dx,Dy,Obj.Circle    );
  else
    Result := Obj;
  end;
end;
(* End of Translate *)


function Translate(const Delta:TGeoFloat; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  Result := Translate(Delta,Delta,Point);
end;
(* End of Translate *)


function Translate(const Delta:TGeoFloat; const Line:TGeoLine2D):TGeoLine2D;
begin
  Result := Translate(Delta,Delta,Line);
end;
(* End of Translate *)


function Translate(const Delta:TGeoFloat; const Segment:TGeoSegment2D):TGeoSegment2D;
begin
  Result := Translate(Delta,Delta,Segment);
end;
(* End of Translate *)


function Translate(const Delta:TGeoFloat; const Triangle:TGeoTriangle2D):TGeoTriangle2D;
begin
  Result := Translate(Delta,Delta,Triangle);
end;
(* End of Translate *)


function Translate(const Delta:TGeoFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result := Translate(Delta,Delta,Quadix);
end;
(* End of Translate *)


function Translate(const Delta:TGeoFloat; const Rectangle:TGeoRectangle):TGeoRectangle;
begin
  Result := Translate(Delta,Delta,Rectangle);
end;
(* End of Translate *)


function Translate(const Delta:TGeoFloat; const Circle:TGeoCircle):TGeoCircle;
begin
  Result := Translate(Delta,Delta,Circle);
end;
(* End of Translate *)


function Translate(const Delta:TGeoFloat; const Polygon:TGeoPolygon2D):TGeoPolygon2D;
begin
  Result := Translate(Delta,Delta,Polygon);
end;
(* End of Translate *)


function Translate(const Delta:TGeoFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  Result := Translate(Delta,Delta,Obj);
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TGeoFloat; const Point:TGeoPoint3D):TGeoPoint3D;
begin
  Result.x := Point.x + Dx;
  Result.y := Point.y + Dy;
  Result.z := Point.z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TGeoFloat; const Line:TGeoLine3D):TGeoLine3D;
begin
  Result[1].x := Line[1].x + Dx;
  Result[1].y := Line[1].y + Dy;
  Result[1].z := Line[1].z + Dz;
  Result[2].x := Line[2].x + Dx;
  Result[2].y := Line[2].y + Dy;
  Result[2].z := Line[2].z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TGeoFloat; const Segment:TGeoSegment3D):TGeoSegment3D;
begin
  Result[1].x := Segment[1].x + Dx;
  Result[1].y := Segment[1].y + Dy;
  Result[1].z := Segment[1].z + Dz;
  Result[2].x := Segment[2].x + Dx;
  Result[2].y := Segment[2].y + Dy;
  Result[2].z := Segment[2].z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TGeoFloat; const Triangle:TGeoTriangle3D):TGeoTriangle3D;
begin
  Result[1].x := Triangle[1].x + Dx;
  Result[1].y := Triangle[1].y + Dy;
  Result[1].z := Triangle[1].z + Dz;
  Result[2].x := Triangle[2].x + Dx;
  Result[2].y := Triangle[2].y + Dy;
  Result[2].z := Triangle[2].z + Dz;
  Result[3].x := Triangle[3].x + Dx;
  Result[3].y := Triangle[3].y + Dy;
  Result[3].z := Triangle[3].z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TGeoFloat; const Quadix:TQuadix3D):TQuadix3D;
begin
  Result[1].x := Quadix[1].x + Dx;
  Result[1].y := Quadix[1].y + Dy;
  Result[1].z := Quadix[1].z + Dz;
  Result[2].x := Quadix[2].x + Dx;
  Result[2].y := Quadix[2].y + Dy;
  Result[2].z := Quadix[2].z + Dz;
  Result[3].x := Quadix[3].x + Dx;
  Result[3].y := Quadix[3].y + Dy;
  Result[3].z := Quadix[3].z + Dz;
  Result[4].x := Quadix[4].x + Dx;
  Result[4].y := Quadix[4].y + Dy;
  Result[4].z := Quadix[4].z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TGeoFloat; const Sphere:TGeoSphere):TGeoSphere;
begin
  Result.x := Sphere.x + Dx;
  Result.y := Sphere.y + Dy;
  Result.z := Sphere.z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TGeoFloat; const Polygon:TGeoPolygon3D):TGeoPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i].x := Polygon[i].x + Dx;
    Result[i].y := Polygon[i].y + Dy;
    Result[i].z := Polygon[i].z + Dz;
  end;
end;
(* End of Translate *)


function Translate(const Point:TGeoPoint3D; const Polygon:TGeoPolygon3D):TGeoPolygon3D;
begin
  Result := Translate(Point.x,Point.y,Point.z,Polygon);
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TGeoFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint3D   : Result.Point3D    := Translate(Dx,Dy,Dz,Obj.Point3D   );
    geoSegment3D : Result.Segment3D  := Translate(Dx,Dy,Dz,Obj.Segment3D );
    geoLine3D    : Result.Line3D     := Translate(Dx,Dy,Dz,Obj.Line3D    );
    geoTriangle3D: Result.Triangle3D := Translate(Dx,Dy,Dz,Obj.Triangle3D);
    geoQuadix3D  : Result.Quadix3D   := Translate(Dx,Dy,Dz,Obj.Quadix3D  );
    geoSphere    : Result.Sphere     := Translate(Dx,Dy,Dz,Obj.Sphere    );
  else
    Result := Obj;
  end;
end;
(* End of Translate *)


function Scale(const Dx,Dy:TGeoFloat; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  Result.x := Point.x * Dx;
  Result.y := Point.y * Dy;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TGeoFloat; const Line:TGeoLine2D):TGeoLine2D;
begin
  Result[1].x := Line[1].x * Dx;
  Result[1].y := Line[1].y * Dy;
  Result[2].x := Line[2].x * Dx;
  Result[2].y := Line[2].y * Dy;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TGeoFloat; const Segment:TGeoSegment2D):TGeoSegment2D;
begin
  Result[1].x := Segment[1].x * Dx;
  Result[1].y := Segment[1].y * Dy;
  Result[2].x := Segment[1].x * Dx;
  Result[2].y := Segment[2].y * Dy;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TGeoFloat; const Triangle:TGeoTriangle2D):TGeoTriangle2D;
begin
  Result[1].x := Triangle[1].x * Dx;
  Result[1].y := Triangle[1].y * Dy;
  Result[2].x := Triangle[2].x * Dx;
  Result[2].y := Triangle[2].y * Dy;
  Result[3].x := Triangle[3].x * Dx;
  Result[3].y := Triangle[3].y * Dy;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TGeoFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1].x := Quadix[1].x * Dx;
  Result[1].y := Quadix[1].y * Dy;
  Result[2].x := Quadix[2].x * Dx;
  Result[2].y := Quadix[2].y * Dy;
  Result[3].x := Quadix[3].x * Dx;
  Result[3].y := Quadix[3].y * Dy;
  Result[4].x := Quadix[4].x * Dx;
  Result[4].y := Quadix[4].y * Dy;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TGeoFloat; const Rectangle:TGeoRectangle):TGeoRectangle;
begin
  Result[1].x := Rectangle[1].x * Dx;
  Result[1].y := Rectangle[1].y * Dy;
  Result[2].x := Rectangle[2].x * Dx;
  Result[2].y := Rectangle[2].y * Dy;
end;
(* End of Scale*)


function Scale(const Dr:TGeoFloat; const Circle:TGeoCircle):TGeoCircle;
begin
  Result.x      := Circle.x;
  Result.y      := Circle.y;
  Result.Radius := Circle.Radius * Dr;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TGeoFloat; const Polygon:TGeoPolygon2D):TGeoPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i].x := Polygon[i].x * Dx;
    Result[i].y := Polygon[i].y * Dy;
  end;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TGeoFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := Scale(Dx,Dy,Obj.Point2D   );
    geoSegment2D : Result.Segment2D  := Scale(Dx,Dy,Obj.Segment2D );
    geoLine2D    : Result.Line2D     := Scale(Dx,Dy,Obj.Line2D    );
    geoTriangle2D: Result.Triangle2D := Scale(Dx,Dy,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := Scale(Dx,Dy,Obj.Quadix2D  );
  else
    Result := Obj;
  end;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TGeoFloat; const Point:TGeoPoint3D):TGeoPoint3D;
begin
  Result.x := Point.x * Dx;
  Result.y := Point.y * Dy;
  Result.z := Point.z * Dz;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TGeoFloat; const Line:TGeoLine3D):TGeoLine3D;
begin
  Result[1].x := Line[1].x * Dx;
  Result[1].y := Line[1].y * Dy;
  Result[1].z := Line[1].z * Dz;
  Result[2].x := Line[2].x * Dx;
  Result[2].y := Line[2].y * Dy;
  Result[2].z := Line[2].z * Dz;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TGeoFloat; const Segment:TGeoSegment3D):TGeoSegment3D;
begin
  Result[1].x := Segment[1].x * Dx;
  Result[1].y := Segment[1].y * Dy;
  Result[1].z := Segment[1].z * Dz;
  Result[2].x := Segment[2].x * Dx;
  Result[2].y := Segment[2].y * Dy;
  Result[2].z := Segment[2].z * Dz;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TGeoFloat; const Triangle:TGeoTriangle3D):TGeoTriangle3D;
begin
  Result[1].x := Triangle[1].x * Dx;
  Result[1].y := Triangle[1].y * Dy;
  Result[1].z := Triangle[1].z * Dz;
  Result[2].x := Triangle[2].x * Dx;
  Result[2].y := Triangle[2].y * Dy;
  Result[2].z := Triangle[2].z * Dz;
  Result[3].x := Triangle[3].x * Dx;
  Result[3].y := Triangle[3].y * Dy;
  Result[3].z := Triangle[3].z * Dz;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TGeoFloat; const Quadix:TQuadix3D):TQuadix3D;
begin
  Result[1].x := Quadix[1].x * Dx;
  Result[1].y := Quadix[1].y * Dy;
  Result[1].z := Quadix[1].z * Dz;
  Result[2].x := Quadix[2].x * Dx;
  Result[2].y := Quadix[2].y * Dy;
  Result[2].z := Quadix[2].z * Dz;
  Result[3].x := Quadix[3].x * Dx;
  Result[3].y := Quadix[3].y * Dy;
  Result[3].z := Quadix[3].z * Dz;
  Result[4].x := Quadix[4].x * Dx;
  Result[4].y := Quadix[4].y * Dy;
  Result[4].z := Quadix[4].z * Dz;
end;
(* End of Scale *)


function Scale(const Dr:TGeoFloat; const Sphere:TGeoSphere):TGeoSphere;
begin
  Result.x      := Sphere.x;
  Result.y      := Sphere.y;
  Result.z      := Sphere.z;
  Result.Radius := Sphere.Radius*Dr;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TGeoFloat; const Polygon:TGeoPolygon3D):TGeoPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i].x := Polygon[i].x * Dx;
    Result[i].y := Polygon[i].y * Dy;
  end;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TGeoFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint3D   : Result.Point3D    := Scale(Dx,Dy,Dz,Obj.Point3D   );
    geoSegment3D : Result.Segment3D  := Scale(Dx,Dy,Dz,Obj.Segment3D );
    geoLine3D    : Result.Line3D     := Scale(Dx,Dy,Dz,Obj.Line3D    );
    geoTriangle3D: Result.Triangle3D := Scale(Dx,Dy,Dz,Obj.Triangle3D);
    geoQuadix3D  : Result.Quadix3D   := Scale(Dx,Dy,Dz,Obj.Quadix3D  );
  else
    Result := Obj;
  end;
end;
(* End of Scale *)


procedure ShearXAxis(const Shear,x,y:TGeoFloat; out Nx,Ny:TGeoFloat);
begin
  Nx := x + Shear * y;
  Ny := y;
end;
(* End of Shear Cartesian Coordiante Along X-Axis *)


function ShearXAxis(const Shear:TGeoFloat; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  ShearXAxis(Shear,Point.x,Point.y,Result.x,Result.y);
end;
(* End of Shear 2D Point Along X-Axis *)


function ShearXAxis(const Shear:TGeoFloat; const Segment:TGeoSegment2D):TGeoSegment2D;
begin
  Result[1] := ShearXAxis(Shear,Segment[1]);
  Result[2] := ShearXAxis(Shear,Segment[2]);
end;
(* End of Shear 2D Segment Along X-Axis *)


function ShearXAxis(const Shear:TGeoFloat; const Triangle:TGeoTriangle2D):TGeoTriangle2D;
begin
  Result[1] := ShearXAxis(Shear,Triangle[1]);
  Result[2] := ShearXAxis(Shear,Triangle[2]);
  Result[3] := ShearXAxis(Shear,Triangle[2]);
end;
(* End of Shear 2D Triangle Along X-Axis *)


function ShearXAxis(const Shear:TGeoFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1] := ShearXAxis(Shear,Quadix[1]);
  Result[2] := ShearXAxis(Shear,Quadix[2]);
  Result[3] := ShearXAxis(Shear,Quadix[3]);
  Result[3] := ShearXAxis(Shear,Quadix[4]);
end;
(* End of Shear 2D Quadix Along X-Axis *)


function ShearXAxis(const Shear:TGeoFloat; const Polygon:TGeoPolygon2D):TGeoPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := ShearXAxis(Shear,Polygon[i]);
  end;
end;
(* End of Shear 2D Polygon Along X-Axis *)


function ShearXAxis(const Shear:TGeoFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := ShearXAxis(Shear,Obj.Point2D   );
    geoSegment2D : Result.Segment2D  := ShearXAxis(Shear,Obj.Segment2D );
    geoTriangle2D: Result.Triangle2D := ShearXAxis(Shear,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := ShearXAxis(Shear,Obj.Quadix2D  );
  else
    Result := Obj;
  end;
end;
(* End of Shear 2D Along X-Axis *)


procedure ShearYAxis(const Shear,x,y:TGeoFloat; out Nx,Ny:TGeoFloat);
begin
  Nx := x;
  Ny := x * Shear + y;
end;
(* End of Shear Cartesian Coordiante Along Y-Axis *)


function ShearYAxis(const Shear:TGeoFloat; const Point:TGeoPoint2D):TGeoPoint2D;
begin
  ShearYAxis(Shear,Point.x,Point.y,Result.x,Result.y);
end;
(* End of Shear 2D Point Along Y-Axis *)


function ShearYAxis(const Shear:TGeoFloat; const Segment:TGeoSegment2D):TGeoSegment2D;
begin
  Result[1] := ShearYAxis(Shear,Segment[1]);
  Result[2] := ShearYAxis(Shear,Segment[2]);
end;
(* End of Shear 2D Segment Along Y-Axis *)


function ShearYAxis(const Shear:TGeoFloat; const Triangle:TGeoTriangle2D):TGeoTriangle2D;
begin
  Result[1] := ShearYAxis(Shear,Triangle[1]);
  Result[2] := ShearYAxis(Shear,Triangle[2]);
  Result[3] := ShearYAxis(Shear,Triangle[2]);
end;
(* End of Shear 2D Triangle Along Y-Axis *)


function ShearYAxis(const Shear:TGeoFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1] := ShearYAxis(Shear,Quadix[1]);
  Result[2] := ShearYAxis(Shear,Quadix[2]);
  Result[3] := ShearYAxis(Shear,Quadix[2]);
  Result[3] := ShearYAxis(Shear,Quadix[2]);
end;
(* End of Shear 2D Quadix Along X-Axis *)


function ShearYAxis(const Shear:TGeoFloat; const Polygon:TGeoPolygon2D):TGeoPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := ShearYAxis(Shear,Polygon[i]);
  end;
end;
(* End of Shear 2D Polygon Along Y-Axis *)


function ShearYAxis(const Shear:TGeoFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := ShearYAxis(Shear,Obj.Point2D   );
    geoSegment2D : Result.Segment2D  := ShearYAxis(Shear,Obj.Segment2D );
    geoTriangle2D: Result.Triangle2D := ShearYAxis(Shear,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := ShearYAxis(Shear,Obj.Quadix2D  );
  else
    Result := Obj;
  end;
end;
(* End of Shear 2D Along Y-Axis *)


function CenterAtLocation(const Point:TGeoPoint2D; const x,y:TGeoFloat):TGeoPoint2D;
begin
  Result.x := x;
  Result.y := y;
end;
(* End Of Center At Location *)


function CenterAtLocation(const Segment:TGeoSegment2D; const x,y:TGeoFloat):TGeoSegment2D;
var
  Cx : TGeoFloat;
  Cy : TGeoFloat;
begin
  SegmentMidPoint(Segment,Cx,Cy);
  Result := Translate(x - Cx,y - Cy,Segment);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Triangle:TGeoTriangle2D; const x,y:TGeoFloat):TGeoTriangle2D;
var
  Cx : TGeoFloat;
  Cy : TGeoFloat;
begin
  Centroid(Triangle,Cx,Cy);
  Result := Translate(x - Cx,y - Cy,Triangle);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Rectangle:TGeoRectangle; const x,y:TGeoFloat):TGeoRectangle;
var
  Cx : TGeoFloat;
  Cy : TGeoFloat;
begin
  Cx     := abs(Rectangle[1].x - Rectangle[2].x) * 0.5;
  Cy     := abs(Rectangle[1].y - Rectangle[2].y) * 0.5;
  Result := Translate(x - Cx,y - Cy,Rectangle);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Quadix:TQuadix2D; const x,y:TGeoFloat):TQuadix2D;
var
  Cx : TGeoFloat;
  Cy : TGeoFloat;
begin
  Centroid(Quadix,Cx,Cy);
  Result := Translate(x - Cx, y - Cy,Quadix);
 (* to Be Completed *)
end;
(* End Of Center At Location *)


function CenterAtLocation(const Circle:TGeoCircle; const x,y:TGeoFloat):TGeoCircle;
begin
  Result.x := x;
  Result.y := y;
end;
(* End Of Center At Location *)


function CenterAtLocation(const Polygon:TGeoPolygon2D; const x,y:TGeoFloat):TGeoPolygon2D;
var
  Cx : TGeoFloat;
  Cy : TGeoFloat;
begin
  Centroid(Polygon,Cx,Cy);
  Result := Translate(x - Cx,y - Cy,Polygon);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Point, CPoint:TGeoPoint2D):TGeoPoint2D;
begin
  Result := CenterAtLocation(Point,CPoint.x,Point.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Segment:TGeoSegment2D; const CPoint:TGeoPoint2D):TGeoSegment2D;
begin
  Result := CenterAtLocation(Segment,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Triangle:TGeoTriangle2D; const CPoint:TGeoPoint2D):TGeoTriangle2D;
begin
  Result := CenterAtLocation(Triangle,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Rectangle:TGeoRectangle; const CPoint:TGeoPoint2D):TGeoRectangle;
begin
  Result := CenterAtLocation(Rectangle,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Quadix:TQuadix2D; const CPoint:TGeoPoint2D):TQuadix2D;
begin
  Result := CenterAtLocation(Quadix,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Circle:TGeoCircle; const CPoint:TGeoPoint2D):TGeoCircle;
begin
  Result := CenterAtLocation(Circle,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Polygon:TGeoPolygon2D; const CPoint:TGeoPoint2D):TGeoPolygon2D;
begin
  Result := CenterAtLocation(Polygon,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function AABB(const Segment : TGeoSegment2D):TGeoRectangle;
begin
  if Segment[1].x < Segment[2].x then
  begin
    Result[1].x := Segment[1].x;
    Result[2].x := Segment[2].x;
  end
  else
  begin
    Result[1].x := Segment[2].x;
    Result[2].x := Segment[1].x;
  end;

  if Segment[1].y < Segment[2].y then
  begin
    Result[1].y := Segment[1].y;
    Result[2].y := Segment[2].y;
  end
  else
  begin
    Result[1].y := Segment[2].y;
    Result[2].y := Segment[1].y;
  end;
end;
(* End Of AABB *)


function AABB(const Triangle : TGeoTriangle2D):TGeoRectangle;
var
  i  : Integer;
begin
  Result[1].x := Triangle[1].x;
  Result[1].y := Triangle[1].y;
  Result[2].x := Triangle[1].x;
  Result[2].y := Triangle[1].y;
  for i := 2 to 3 do
  begin
    if Triangle[i].x < Result[1].x then
      Result[1].x := Triangle[i].x
    else if Triangle[i].x > Result[2].x then
      Result[2].x := Triangle[i].x;
    if Triangle[i].y < Result[1].y then
      Result[1].y := Triangle[i].y
    else if Triangle[i].y > Result[2].y then
      Result[2].y := Triangle[i].y;
  end;
end;
(* End Of AABB *)


function AABB(const Rectangle : TGeoRectangle):TGeoRectangle;
begin
  Result[1].x := Min(Rectangle[1].x,Rectangle[2].x);
  Result[1].y := Min(Rectangle[1].y,Rectangle[2].y);
  Result[2].x := Max(Rectangle[1].x,Rectangle[2].x);
  Result[2].y := Max(Rectangle[1].y,Rectangle[2].y);
end;
(* End Of AABB *)


function AABB(const Quadix : TQuadix2D):TGeoRectangle;
var
  i  : Integer;
begin
  Result[1].x := Quadix[1].x;
  Result[1].y := Quadix[1].y;
  Result[2].x := Quadix[1].x;
  Result[2].y := Quadix[1].y;
  for i := 2 to 4 do
  begin
    if Quadix[i].x < Result[1].x then
      Result[1].x := Quadix[i].x
    else if Quadix[i].x > Result[2].x then
      Result[2].x := Quadix[i].x;
    if Quadix[i].y < Result[1].y then
      Result[1].y := Quadix[i].y
    else if Quadix[i].y > Result[2].y then
      Result[2].y := Quadix[i].y;
  end;
end;
(* End Of AABB *)


function AABB(const Circle : TGeoCircle):TGeoRectangle;
begin
  Result[1].x := Circle.x - Circle.Radius;
  Result[1].y := Circle.y - Circle.Radius;
  Result[2].x := Circle.x + Circle.Radius;
  Result[2].y := Circle.y + Circle.Radius;
end;
(* End Of AABB *)


function AABB(const Polygon : TGeoPolygon2D):TGeoRectangle;
var
  i  : Integer;
begin
  Result[1].x := Polygon[0].x;
  Result[1].y := Polygon[0].y;
  Result[2].x := Polygon[0].x;
  Result[2].y := Polygon[0].y;
  for i := 1 to Length(Polygon) - 1 do
  begin
    if Polygon[i].x < Result[1].x then
      Result[1].x := Polygon[i].x
    else if Polygon[i].x > Result[2].x then
      Result[2].x := Polygon[i].x;
    if Polygon[i].y < Result[1].y then
      Result[1].y := Polygon[i].y
    else if Polygon[i].y > Result[2].y then
      Result[2].y := Polygon[i].y;
  end;
end;
(* End Of AABB *)


function AABB(const Curve : TGeoPoint2DArray):TGeoRectangle;
var
  i  : Integer;
begin
  Result[1].x := Curve[0].x;
  Result[1].y := Curve[0].y;
  Result[2].x := Curve[0].x;
  Result[2].y := Curve[0].y;
  for i := 1 to Length(Curve) - 1 do
  begin
    if Curve[i].x < Result[1].x then
      Result[1].x := Curve[i].x
    else if Curve[i].x > Result[2].x then
      Result[2].x := Curve[i].x;
    if Curve[i].y < Result[1].y then
      Result[1].y := Curve[i].y
    else if Curve[i].y > Result[2].y then
      Result[2].y := Curve[i].y;
  end;
end;
(* End Of AABB *)


procedure AABB(const Segment : TGeoSegment2D; out x1,y1,x2,y2:TGeoFloat);
var
   Rectangle : TGeoRectangle;
begin
  Rectangle := AABB(Segment);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)


procedure AABB(const Triangle : TGeoTriangle2D; out x1,y1,x2,y2:TGeoFloat);
var
   Rectangle : TGeoRectangle;
begin
  Rectangle := AABB(Triangle);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)

procedure AABB(const Rectangle : TGeoRectangle;  out x1,y1,x2,y2:TGeoFloat);
var
   _Rectangle : TGeoRectangle;
begin
  _Rectangle := AABB(Rectangle);
  x1 := _Rectangle[1].x;
  y1 := _Rectangle[1].y;
  x2 := _Rectangle[2].x;
  y2 := _Rectangle[2].y;
end;
(* End Of AABB *)


procedure AABB(const Quadix : TQuadix2D; out x1,y1,x2,y2:TGeoFloat);
var
   Rectangle : TGeoRectangle;
begin
  Rectangle := AABB(Quadix);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)


procedure AABB(const Circle : TGeoCircle; out x1,y1,x2,y2:TGeoFloat);
var
   Rectangle : TGeoRectangle;
begin
  Rectangle := AABB(Circle);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)


procedure AABB(const Polygon : TGeoPolygon2D; out x1,y1,x2,y2:TGeoFloat);
var
   Rectangle : TGeoRectangle;
begin
  Rectangle := AABB(Polygon);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)


procedure AABB(const Curve : TGeoPoint2DArray; out x1,y1,x2,y2:TGeoFloat);
var
   Rectangle : TGeoRectangle;
begin
  Rectangle := AABB(Curve);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)


procedure ProjectPoint(const Srcx,Srcy,Dstx,Dsty,Dist:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  DistRatio : TGeoFloat;
begin
  DistRatio := Dist / Distance(Srcx,Srcy,Dstx,Dsty);
  Nx := Srcx + DistRatio * (Dstx - SrcX);
  Ny := Srcy + DistRatio * (Dsty - Srcy);
end;
(* End of Project Point 2D *)


procedure ProjectPoint(const Srcx,Srcy,Srcz,Dstx,Dsty,Dstz,Dist:TGeoFloat; out Nx,Ny,Nz:TGeoFloat);
var
  DistRatio : TGeoFloat;
begin
  DistRatio := Dist / Distance(Srcx,Srcy,Srcz,Dstx,Dsty,Dstz);
  Nx := Srcx + DistRatio * (Dstx - SrcX);
  Ny := Srcy + DistRatio * (Dsty - Srcy);
  Nz := Srcz + DistRatio * (Dstz - Srcz);
end;
(* End of Project Point 3D *)


procedure ProjectPoint(const Px,Py,Angle,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  Dx : TGeoFloat;
  Dy , AA: TGeoFloat;
begin
  Dx := Zero;
  Dy := Zero;
  case Quadrant(Angle) of
    1 : begin
          AA := Angle * PIDiv180;
          Dx := Cos(AA) * Distance;
          Dy := Sin(AA) * Distance;
        end;
    2 : begin
          AA := (Angle - 90.0) * PIDiv180;
          Dx := Sin(AA) * Distance * -1.0;
          Dy := Cos(AA) * Distance;
        end;
    3 : begin
          AA := (Angle - 180.0) * PIDiv180;
          Dx := Cos(AA) * Distance * -1.0;
          Dy := Sin(AA) * Distance * -1.0;
        end;
    4 : begin
          AA := (Angle - 270.0) * PIDiv180;
          Dx := Sin(AA) * Distance;
          Dy := Cos(AA) * Distance * -1.0;
        end;
   end;
   Nx := Px + Dx;
   Ny := Py + Dy;
end;
(* End of Project Point 2D *)


procedure ProjectPoint0(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);
begin
  Nx := Px + Distance;
  Ny := Py;
end;
(* End of Project Point 2D *)


procedure ProjectPoint45(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  aa: TGeoFloat;
begin
  aa := SQRT_OF_TWO * Distance;
  Nx := Px + aa;
  Ny := Py + aa;
end;
(* End of Project Point 2D *)


procedure ProjectPoint90(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);
begin
  Nx := Px;
  Ny := Py + Distance;
end;
(* End of Project Point 2D *)


procedure ProjectPoint135(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  aa: TGeoFloat;
begin
  aa := SQRT_OF_TWO * Distance;
  Nx := Px - aa;
  Ny := Py + aa;
end;
(* End of Project Point 2D *)


procedure ProjectPoint180(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);
begin
  Nx := Px - Distance;
  Ny := Py;
end;
(* End of Project Point 2D *)


procedure ProjectPoint225(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  aa: TGeoFloat;
begin
  aa := SQRT_OF_TWO * Distance;
  Nx := Px - aa;
  Ny := Py - aa;
end;
(* End of Project Point 2D *)


procedure ProjectPoint270(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);
begin
  Nx := Px;
  Ny := Py - Distance;
end;
(* End of Project Point 2D *)


procedure ProjectPoint315(const Px,Py,Distance:TGeoFloat; out Nx,Ny:TGeoFloat);
var
  aa: TGeoFloat;
begin
  aa := SQRT_OF_TWO * Distance;
  Nx := Px + aa;
  Ny := Py - aa;
end;
(* End of Project Point 2D *)


function ProjectPoint(const SrcPoint,DstPoint:TGeoPoint2D; const Dist:TGeoFloat):TGeoPoint2D;
begin
  ProjectPoint(SrcPoint.x,SrcPoint.y,DstPoint.x,DstPoint.y,Dist,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint(const SrcPoint,DstPoint:TGeoPoint3D; const Dist:TGeoFloat):TGeoPoint3D;
begin
  ProjectPoint(SrcPoint.x,SrcPoint.y,SrcPoint.z,DstPoint.x,DstPoint.y,DstPoint.z,Dist,Result.x,Result.y,Result.z);
end;
(* End of Project Point 3D *)


function ProjectPoint(const Point:TGeoPoint2D; Angle,Distance:TGeoFloat):TGeoPoint2D;
begin
  ProjectPoint(Point.x,Point.y,Angle,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint0(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;
begin
  ProjectPoint0(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint45(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;
begin
  ProjectPoint45(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint90(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;
begin
  ProjectPoint90(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint135(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;
begin
  ProjectPoint135(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint180(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;
begin
  ProjectPoint180(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint225(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;
begin
  ProjectPoint225(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint270(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;
begin
  ProjectPoint270(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint315(const Point:TGeoPoint2D; const Distance:TGeoFloat):TGeoPoint2D;
begin
  ProjectPoint315(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectObject(const Point:TGeoPoint2D; const Angle,Distance:TGeoFloat):TGeoPoint2D;
begin
  Result := ProjectPoint(Point,Angle,Distance);
end;
(* End of Project Object *)


function ProjectObject(const Segment:TGeoSegment2D; const Angle,Distance:TGeoFloat):TGeoSegment2D;
begin
  Result[1] := ProjectPoint(Segment[1],Angle,Distance);
  Result[2] := ProjectPoint(Segment[2],Angle,Distance);
end;
(* End of Project Object *)


function ProjectObject(const Triangle:TGeoTriangle2D; const Angle,Distance:TGeoFloat):TGeoTriangle2D;
begin
  Result[1] := ProjectPoint(Triangle[1],Angle,Distance);
  Result[2] := ProjectPoint(Triangle[2],Angle,Distance);
  Result[3] := ProjectPoint(Triangle[3],Angle,Distance);
end;
(* End of Project Object *)


function ProjectObject(const Quadix:TQuadix2D; const Angle,Distance:TGeoFloat):TQuadix2D;
begin
  Result[1] := ProjectPoint(Quadix[1],Angle,Distance);
  Result[2] := ProjectPoint(Quadix[2],Angle,Distance);
  Result[3] := ProjectPoint(Quadix[3],Angle,Distance);
  Result[4] := ProjectPoint(Quadix[4],Angle,Distance);
end;
(* End of Project Object *)


function ProjectObject(const Circle:TGeoCircle; const Angle,Distance:TGeoFloat):TGeoCircle;
begin
  ProjectPoint(Circle.x,Circle.y,Angle,Distance,Result.x,Result.y);
end;
(* End of Project Object *)


function ProjectObject(const Polygon : TGeoPolygon2D; const Angle,Distance:TGeoFloat):TGeoPolygon2D;
var
  i : Integer;
begin
  Setlength(Result,Length(Polygon));
  for i := 0 to Length(Result) -  1 do
  begin
    Result[i] := ProjectObject(Polygon[i],Angle,Distance);
  end;
end;
(* End of Project Object *)


function ProjectObject(const GeoObj : TGeometricObject; const Angle,Distance:TGeoFloat):TGeometricObject;
begin
  case GeoObj.ObjectType of
    geoPoint2D   : Result.Point2D    := ProjectObject(GeoObj.Point2D   ,Angle,Distance);
    geoSegment2D : Result.Segment2D  := ProjectObject(GeoObj.Segment2D ,Angle,Distance);
    geoTriangle2D: Result.Triangle2D := ProjectObject(GeoObj.Triangle2D,Angle,Distance);
    geoQuadix2D  : Result.Quadix2D   := ProjectObject(GeoObj.Quadix2D  ,Angle,Distance);
    geoCircle    : Result.Circle     := ProjectObject(GeoObj.Circle    ,Angle,Distance);
  else
    Result := GeoObj;
  end;
end;
(* End of Project Object *)


procedure CalculateBezierCoefficients(const Bezier:TGeoQuadraticBezier2D; out ax,bx,ay,by:TGeoFloat);
begin
  bx := 2.0 * (Bezier[1].x - Bezier[0].x);
  by := 2.0 * (Bezier[1].y - Bezier[0].y);
  ax := Bezier[2].x - Bezier[0].x - bx;
  ay := Bezier[2].y - Bezier[0].y - by;
end;
(* End of Calculate Bezier Values *)


procedure CalculateBezierCoefficients(const Bezier:TGeoQuadraticBezier3D; out ax,bx,ay,by,az,bz:TGeoFloat);
begin
  bx := 2.0 * (Bezier[1].x - Bezier[0].x);
  by := 2.0 * (Bezier[1].y - Bezier[0].y);
  bz := 2.0 * (Bezier[1].z - Bezier[0].z);
  ax := Bezier[2].x - Bezier[0].x - bx;
  ay := Bezier[2].y - Bezier[0].y - by;
  az := Bezier[2].z - Bezier[0].z - bz;
end;
(* End of Calculate Bezier Values *)


procedure CalculateBezierCoefficients(const Bezier:TGeoCubicBezier2D; out ax,bx,cx,ay,by,cy:TGeoFloat);
begin
  cx := 3.0 * (Bezier[1].x - Bezier[0].x);
  cy := 3.0 * (Bezier[1].y - Bezier[0].y);
  bx := 3.0 * (Bezier[2].x - Bezier[1].x) - cx;
  by := 3.0 * (Bezier[2].y - Bezier[1].y) - cy;
  ax := Bezier[3].x - Bezier[0].x - cx - bx;
  ay := Bezier[3].y - Bezier[0].y - cy - by;
end;
(* End of Calculate Bezier Values *)


procedure CalculateBezierCoefficients(const Bezier:TGeoCubicBezier3D; out ax,bx,cx,ay,by,cy,az,bz,cz:TGeoFloat);
begin
  cx := 3.0 * (Bezier[1].x - Bezier[0].x);
  cy := 3.0 * (Bezier[1].y - Bezier[0].y);
  cz := 3.0 * (Bezier[1].z - Bezier[0].z);
  bx := 3.0 * (Bezier[2].x - Bezier[1].x) - cx;
  by := 3.0 * (Bezier[2].y - Bezier[1].y) - cy;
  bz := 3.0 * (Bezier[2].z - Bezier[1].z) - cz;
  ax := Bezier[3].x - Bezier[0].x - cx - bx;
  ay := Bezier[3].y - Bezier[0].y - cy - by;
  az := Bezier[3].z - Bezier[0].z - cz - bz;
end;
(* End of Calculate Bezier Values *)


function PointOnBezier(const StartPoint:TGeoPoint2D; const ax,bx,ay,by,T:TGeoFloat):TGeoPoint2D;
var
  tSqr  : TGeoFloat;
begin
  tSqr     := t * t;
  Result.x := (ax * tSqr) + (bx * t) + StartPoint.x;
  Result.y := (ay * tSqr) + (by * t) + StartPoint.y;
end;
(* End of Point On Bezier *)


function PointOnBezier(const StartPoint:TGeoPoint3D; const ax,bx,ay,by,az,bz,T:TGeoFloat):TGeoPoint3D;
var
  tSqr  : TGeoFloat;
begin
  tSqr     := t * t;
  Result.x := (ax * tSqr) + (bx * t) + StartPoint.x;
  Result.y := (ay * tSqr) + (by * t) + StartPoint.y;
  Result.z := (az * tSqr) + (bz * t) + StartPoint.z;
end;
(* End of Point On Bezier *)


function PointOnBezier(const StartPoint:TGeoPoint2D; const ax,bx,cx,ay,by,cy,T:TGeoFloat):TGeoPoint2D;
var
  tSqr  : TGeoFloat;
  tCube : TGeoFloat;
begin
  tSqr     := t * t;
  tCube    := tSqr * t;
  Result.x := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.x;
  Result.y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.y;
end;
(* End of Point On Bezier *)


function PointOnBezier(const StartPoint:TGeoPoint3D; const ax,bx,cx,ay,by,cy,az,bz,cz,T:TGeoFloat):TGeoPoint3D;
var
  tSqr  : TGeoFloat;
  tCube : TGeoFloat;
begin
  tSqr     := t * t;
  tCube    := tSqr * t;
  Result.x := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.x;
  Result.y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.y;
  Result.z := (az * tCube) + (bz * tSqr) + (cz * t) + StartPoint.z;
end;
(* End of Point On Bezier *)


function CreateBezier(const Bezier:TGeoQuadraticBezier2D; const PointCount:Integer) : TGeoPoint2DArray;
var
  ax : TGeoFloat;
  ay : TGeoFloat;
  bx : TGeoFloat;
  by : TGeoFloat;
  dT : TGeoFloat;
  T  : TGeoFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := Zero;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,ay,by);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := PointOnBezier(Bezier[0],ax,bx,ay,by,T);
    T := T + dT;
  end;
end;
(* End of Create Bezier *)


function CreateBezier(const Bezier:TGeoQuadraticBezier3D; const PointCount:Integer) : TGeoPoint3DArray;
var
  ax : TGeoFloat;
  ay : TGeoFloat;
  az : TGeoFloat;
  bx : TGeoFloat;
  by : TGeoFloat;
  bz : TGeoFloat;
  dT : TGeoFloat;
  T  : TGeoFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := Zero;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,ay,by,az,bz);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := PointOnBezier(Bezier[0],ax,bx,ay,by,az,bz,T);
    T := T + dT;
  end;
end;
(* End of Create Bezier *)


function CreateBezier(const Bezier:TGeoCubicBezier2D; const PointCount:Integer) : TGeoPoint2DArray;
var
  ax : TGeoFloat;
  bx : TGeoFloat;
  cx : TGeoFloat;
  ay : TGeoFloat;
  by : TGeoFloat;
  cy : TGeoFloat;
  dT : TGeoFloat;
  T  : TGeoFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := Zero;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,cx,ay,by,cy);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := PointOnBezier(Bezier[0],ax,bx,cx,ay,by,cy,T);
    T := T + dT;
  end;
end;
(* End of Create Bezier *)


function CreateBezier(const Bezier:TGeoCubicBezier3D; const PointCount:Integer) : TGeoPoint3DArray;
var
  ax : TGeoFloat;
  bx : TGeoFloat;
  cx : TGeoFloat;
  ay : TGeoFloat;
  by : TGeoFloat;
  cy : TGeoFloat;
  az : TGeoFloat;
  bz : TGeoFloat;
  cz : TGeoFloat;
  dT : TGeoFloat;
  T  : TGeoFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := Zero;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,cx,ay,by,cy,az,bz,cz);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := PointOnBezier(Bezier[0],ax,bx,cx,ay,by,cy,az,bz,cz,T);
    T := T + dT;
  end;
end;
(* End of Create Bezier *)


function CreateCurvePointBezier(const Bezier:TGeoQuadraticBezier2D; const PointCount:Integer):TGeoCurvePoint2DArray;
var
  ax : TGeoFloat;
  ay : TGeoFloat;
  bx : TGeoFloat;
  by : TGeoFloat;
  dT : TGeoFloat;
  T  : TGeoFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := Zero;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,ay,by);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := EquateCurvePoint(PointOnBezier(Bezier[0],ax,bx,ay,by,T),T);
    T := T + dT;
  end;
end;
(* End of Create Curve Point Bezier *)


function CreateCurvePointBezier(const Bezier:TGeoQuadraticBezier3D; const PointCount:Integer):TGeoCurvePoint3DArray;
var
  ax : TGeoFloat;
  ay : TGeoFloat;
  az : TGeoFloat;
  bx : TGeoFloat;
  by : TGeoFloat;
  bz : TGeoFloat;
  dT : TGeoFloat;
  T  : TGeoFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := Zero;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,ay,by,az,bz);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := EquateCurvePoint(PointOnBezier(Bezier[0],ax,bx,ay,by,az,bz,T),T);
    T := T + dT;
  end;
end;
(* End of Create Curve Point Bezier *)


function CreateCurvePointBezier(const Bezier:TGeoCubicBezier2D; const PointCount:Integer):TGeoCurvePoint2DArray;
var
  ax : TGeoFloat;
  bx : TGeoFloat;
  cx : TGeoFloat;
  ay : TGeoFloat;
  by : TGeoFloat;
  cy : TGeoFloat;
  dT : TGeoFloat;
  T  : TGeoFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := Zero;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,cx,ay,by,cy);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := EquateCurvePoint(PointOnBezier(Bezier[0],ax,bx,cx,ay,by,cy,T),T);
    T := T + dT;
  end;
end;
(* End of Create Curve Point Bezier *)


function CreateCurvePointBezier(const Bezier:TGeoCubicBezier3D; const PointCount:Integer):TGeoCurvePoint3DArray;
var
  ax : TGeoFloat;
  bx : TGeoFloat;
  cx : TGeoFloat;
  ay : TGeoFloat;
  by : TGeoFloat;
  cy : TGeoFloat;
  az : TGeoFloat;
  bz : TGeoFloat;
  cz : TGeoFloat;
  dT : TGeoFloat;
  T  : TGeoFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := Zero;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,cx,ay,by,cy,az,bz,cz);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := EquateCurvePoint(PointOnBezier(Bezier[0],ax,bx,cx,ay,by,cy,az,bz,cz,T),T);
    T := T + dT;
  end;
end;
(* End of Create Curve Point Bezier *)


function CurveLength(const Bezier:TGeoQuadraticBezier2D; const PointCount:Integer):TGeoFloat;
var
   i     : Integer;
   Curve : TGeoPoint2DArray;
begin
  Result := 0;
  Curve  := CreateBezier(Bezier,PointCount);
  for i := 0 To Length(Curve) - 2 do
  begin
    Result := Result + Distance(Curve[i],Curve[i + 1]);
  end;
  Finalize(Curve);
end;
(* End of Curve Length *)


function CurveLength(const Bezier:TGeoQuadraticBezier3D; const PointCount:Integer):TGeoFloat;
var
   i     : Integer;
   Curve : TGeoPoint3DArray;
begin
  Result := 0;
  Curve  := CreateBezier(Bezier,PointCount);
  for i := 0 To Length(Curve) - 2 do
  begin
    Result := Result + Distance(Curve[i],Curve[i + 1]);
  end;
  Finalize(Curve);
end;
(* End of Curve Length *)


function CurveLength(const Bezier:TGeoCubicBezier2D; const PointCount:Integer):TGeoFloat;
var
   i     : Integer;
   Curve : TGeoPoint2DArray;
begin
  Result := 0;
  Curve  := CreateBezier(Bezier,PointCount);
  for i := 0 To Length(Curve) - 2 do
  begin
    Result := Result + Distance(Curve[i],Curve[i + 1]);
  end;
  Finalize(Curve);
end;
(* End of Curve Length *)


function CurveLength(const Bezier:TGeoCubicBezier3D; const PointCount:Integer):TGeoFloat;
var
   i     : Integer;
   Curve : TGeoPoint3DArray;
begin
  Result := 0;
  Curve  := CreateBezier(Bezier,PointCount);
  for i := 0 To Length(Curve) - 2 do
  begin
    Result := Result + Distance(Curve[i],Curve[i + 1]);
  end;
  Finalize(Curve);
end;
(* End of Curve Length *)


procedure ShortenSegment(const Amount:TGeoFloat; var x1,y1,x2,y2:TGeoFloat);
var
  SegmentLength : TGeoFloat;
  DistRatio     : TGeoFloat;
  Dx            : TGeoFloat;
  Dy            : TGeoFloat;
begin
  SegmentLength := Distance(x1,y1,x2,y2);

  if SegmentLength < Amount then
  begin
    SegmentMidPoint(x1,y1,x2,y2,x1,y1);
    x2 := x1;
    y2 := y1;
    Exit;
  end;

  DistRatio := Amount / (2 * SegmentLength);
  Dx        := x2 - x1;
  Dy        := y2 - y1;

  x1 := x1 + DistRatio * Dx;
  y1 := y1 + DistRatio * Dy;
  x2 := x2 - DistRatio * Dx;
  y2 := y2 - DistRatio * Dy;
end;
(* End of Shorten Segment *)


procedure ShortenSegment(const Amount:TGeoFloat; var x1,y1,z1,x2,y2,z2:TGeoFloat);
var
  SegmentLength : TGeoFloat;
  DistRatio     : TGeoFloat;
  Dx            : TGeoFloat;
  Dy            : TGeoFloat;
  Dz            : TGeoFloat;
begin
  SegmentLength := Distance(x1,y1,z1,x2,y2,z2);

  if SegmentLength <= Amount then
  begin
    SegmentMidPoint(x1,y1,z1,x2,y2,z2,x1,y1,z1);
    x2 := x1;
    y2 := y1;
    z2 := z1;
    Exit;
  end;

  DistRatio := Amount / (2 * SegmentLength);
  Dx        := x2 - x1;
  Dy        := y2 - y1;
  Dz        := z2 - z1;

  x1 := x1 + DistRatio * Dx;
  y1 := y1 + DistRatio * Dy;
  z1 := z1 + DistRatio * Dz;
  x2 := x2 - DistRatio * Dx;
  y2 := y2 - DistRatio * Dy;
  z2 := z2 - DistRatio * Dz;
end;
(* End of Shorten Segment *)


function ShortenSegment(const Segment:TGeoSegment2D; const Amount : TGeoFloat):TGeoSegment2D;
begin
  Result := Segment;
  ShortenSegment(Amount,Result[1].x,Result[1].y,Result[2].x,Result[2].y);
end;
(* End of Shorten Segment *)


function ShortenSegment(const Segment:TGeoSegment3D; const Amount : TGeoFloat):TGeoSegment3D;
begin
  Result := Segment;
  ShortenSegment(Amount,Result[1].x,Result[1].y,Result[1].z,Result[2].x,Result[2].y,Result[2].z);
end;
(* End of Shorten Segment *)


procedure LengthenSegment(const Amount:TGeoFloat; out x1,y1,x2,y2:TGeoFloat);
var
  Cx            : TGeoFloat;
  Cy            : TGeoFloat;
  SegmentLength : TGeoFloat;
  Ratio         : TGeoFloat;
begin
  SegmentLength := Distance(x1,y1,x2,y2);
  SegmentMidPoint(x1,y1,x2,y2,cx,cy);
  Ratio := (Amount + SegmentLength) / SegmentLength;
  x1 := cx + Ratio * (x1 - cx);
  y1 := cy + Ratio * (y1 - cy);
  x2 := cx + Ratio * (x2 - cx);
  y2 := cy + Ratio * (y2 - cy);
end;
(* End of Lengthen Segment *)


procedure LengthenSegment(const Amount:TGeoFloat; out x1,y1,z1,x2,y2,z2:TGeoFloat);
var
  SegmentLength : TGeoFloat;
  DistRatio     : TGeoFloat;
  Dx            : TGeoFloat;
  Dy            : TGeoFloat;
  Dz            : TGeoFloat;
begin
  SegmentLength := Distance(x1,y1,z1,x2,y2,z2);

  DistRatio := Amount / (2 * SegmentLength);
  Dx        := x2 - x1;
  Dy        := y2 - y1;
  Dz        := z2 - z1;

  x1 := x1 - DistRatio * Dx;
  y1 := y1 - DistRatio * Dy;
  z1 := z1 - DistRatio * Dz;
  x2 := x2 + DistRatio * Dx;
  y2 := y2 + DistRatio * Dy;
  z2 := z2 + DistRatio * Dz;
end;
(* End of Lengthen Segment *)


function LengthenSegment(const Segment:TGeoSegment2D; const Amount:TGeoFloat):TGeoSegment2D;
begin
  Result := Segment;
  LengthenSegment(Amount,Result[1].x,Result[1].y,Result[2].x,Result[2].y);
end;
(* End of Lengthen Segment *)


function LengthenSegment(const Segment:TGeoSegment3D; const Amount:TGeoFloat):TGeoSegment3D;
begin
  Result := Segment;
  LengthenSegment(Amount,Result[1].x,Result[1].y,Result[1].z,Result[2].x,Result[2].y,Result[2].z);
end;
(* End of Lengthen Segment *)


function EquatePoint(const x,y:TGeoFloat):TGeoPoint2D;
begin
  Result.x := x;
  Result.y := y;
end;
(* End of Equate Point *)


function EquatePoint(const x,y,z:TGeoFloat):TGeoPoint3D;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;
(* End of Equate Point *)


function EquatePointPtr(const x,y:TGeoFloat):TGeoPoint2DPtr;
begin
  New(Result);
  Result^ := EquatePoint(x,y);
end;
(* End of Equate Point *)


function EquatePointPtr(const x,y,z:TGeoFloat):TGeoPoint3DPtr;
begin
  New(Result);
  Result^ := EquatePoint(x,y,z);
end;
(* End of Equate Point *)


procedure EquatePoint(const x,y:TGeoFloat; out Point:TGeoPoint2D);
begin
  Point.x := x;
  Point.y := y;
end;
(* End of Equate Point *)


procedure EquatePoint(const x,y,z:TGeoFloat; out Point:TGeoPoint3D);
begin
  Point.x := x;
  Point.y := y;
  Point.z := z;
end;
(* End of Equate Point *)


procedure EquatePointPtr(const x,y:TGeoFloat; out Point:TGeoPoint2DPtr);
begin
 Point := EquatePointPtr(x,y);
end;
(* End of Equate Point *)


procedure EquatePointPtr(const x,y,z:TGeoFloat; out Point:TGeoPoint3DPtr);
begin
 Point := EquatePointPtr(x,y,z);
end;
(* End of Equate Point *)


function EquateCurvePoint(x,y,t:TGeoFloat):TGeoCurvePoint2D;
begin
  Result.x := x;
  Result.y := y;
  Result.t := t;
end;
(* End of Equate Curve Point *)


function EquateCurvePoint(x,y,z,t:TGeoFloat):TGeoCurvePoint3D;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.t := t;
end;
(* End of Equate Curve Point *)


function EquateCurvePoint(Point:TGeoPoint2D; t:TGeoFloat):TGeoCurvePoint2D;
begin
  Result.x := Point.x;
  Result.y := Point.y;
  Result.t := t;
end;
(* End of Equate Curve Point *)


function EquateCurvePoint(Point:TGeoPoint3D; t:TGeoFloat):TGeoCurvePoint3D;
begin
  Result.x := Point.x;
  Result.y := Point.y;
  Result.z := Point.z;
  Result.t := t;
end;
(* End of Equate Curve Point *)


function EquateSegment(const x1,y1,x2,y2:TGeoFloat):TGeoSegment2D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[2].x := x2;
  Result[2].y := y2;
end;
(* End of Equate Segment *)


function EquateSegment(const Point1,Point2:TGeoPoint2D):TGeoSegment2D;
begin
  Result := EquateSegment(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of Equate Segment *)


function EquateSegment(const Point1,Point2:TGeoPoint3D):TGeoSegment3D;
begin
  Result := EquateSegment(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of Equate Segment *)


function EquateSegment(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoSegment3D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[1].z := z1;
  Result[2].x := x2;
  Result[2].y := y2;
  Result[2].z := z2;
end;
(* End of EquateSegment *)


procedure EquateSegment(const x1,y1,x2,y2:TGeoFloat; out Segment:TGeoSegment2D);
begin
  Segment[1].x := x1;
  Segment[1].y := y1;
  Segment[2].x := x2;
  Segment[2].y := y2;
end;
(* End of Equate Segment *)


procedure EquateSegment(const x1,y1,z1,x2,y2,z2:TGeoFloat; out Segment:TGeoSegment3D);
begin
  Segment[1].x := x1;
  Segment[1].y := y1;
  Segment[1].z := z1;
  Segment[2].x := x2;
  Segment[2].y := y2;
  Segment[2].z := z2;
end;
(* End of Equate Segment *)


function EquateLine(const x1,y1,x2,y2:TGeoFloat):TGeoLine2D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[2].x := x2;
  Result[2].y := y2;
end;
(* End of Equate Segment *)


function EquateLine(const x1,y1,z1,x2,y2,z2:TGeoFloat):TGeoLine3D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[1].z := z1;
  Result[2].x := x2;
  Result[2].y := y2;
  Result[2].z := z2;
end;
(* End of Equate Segment *)


function EquateLine(const Point1,Point2:TGeoPoint2D):TGeoLine2D;
begin
  Result := EquateLine(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of Equate Segment *)


function EquateLine(const Point1,Point2:TGeoPoint3D):TGeoLine3D;
begin
  Result := EquateLine(Point1.x,Point1.y,Point1.z,Point1.x,Point2.y,Point2.z);
end;
(* End of Equate Segment *)


procedure EquateLine(const x1,y1,x2,y2:TGeoFloat; out Line:TGeoLine2D);
begin
  Line[1].x := x1;
  Line[1].y := y1;
  Line[2].x := x2;
  Line[2].y := y2;
end;
(* End of Equate Segment *)


procedure EquateLine(const x1,y1,z1,x2,y2,z2:TGeoFloat; out Line:TGeoLine3D);
begin
  Line[1].x := x1;
  Line[1].y := y1;
  Line[1].z := z1;
  Line[2].x := x2;
  Line[2].y := y2;
  Line[2].z := z2;
end;
(* End of Equate Segment *)


function EquateQuadix(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):TQuadix2D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[2].x := x2;
  Result[2].y := y2;
  Result[3].x := x3;
  Result[3].y := y3;
  Result[4].x := x4;
  Result[4].y := y4;
end;
(* End of Equate Quadix *)


function EquateQuadix(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat):TQuadix3D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[1].z := z1;
  Result[2].x := x2;
  Result[2].y := y2;
  Result[2].z := z2;
  Result[3].x := x3;
  Result[3].y := y3;
  Result[3].z := z3;
  Result[4].x := x4;
  Result[4].y := y4;
  Result[4].z := z4;
end;
(* End of Equate Quadix *)


function EquateQuadix(const Point1,Point2,Point3,Point4:TGeoPoint2D):TQuadix2D;
begin
  Result := EquateQuadix(
                         Point1.x,Point1.y,
                         Point2.x,Point2.y,
                         Point3.x,Point3.y,
                         Point4.x,Point4.y
                        );
end;
(* End of Equate Quadix *)


function EquateQuadix(const Point1,Point2,Point3,Point4:TGeoPoint3D):TQuadix3D;
begin
  Result := EquateQuadix(
                         Point1.x,Point1.y,Point1.z,
                         Point2.x,Point2.y,Point1.z,
                         Point3.x,Point3.y,Point1.z,
                         Point4.x,Point4.y,Point1.z
                        );
end;
(* End of Equate Quadix *)


procedure EquateQuadix(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; out Quadix:TQuadix2D);
begin
  Quadix[1].x := x1;
  Quadix[1].y := y1;
  Quadix[2].x := x2;
  Quadix[2].y := y2;
  Quadix[3].x := x3;
  Quadix[3].y := y3;
  Quadix[4].x := x4;
  Quadix[4].y := y4;
end;
(* End of Equate Quadix *)


procedure EquateQuadix(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; out Quadix:TQuadix3D);
begin
  Quadix[1].x := x1;
  Quadix[1].y := y1;
  Quadix[1].z := z1;
  Quadix[2].x := x2;
  Quadix[2].y := y2;
  Quadix[2].z := z2;
  Quadix[3].x := x3;
  Quadix[3].y := y3;
  Quadix[3].z := z3;
  Quadix[4].x := x4;
  Quadix[4].y := y4;
  Quadix[4].z := z4;
end;
(* End of Equate Quadix *)


function EquateRectangle(const x1,y1,x2,y2:TGeoFloat):TGeoRectangle;
begin
  if x1 <= x2 then
  begin
    Result[1].x := x1;
    Result[2].x := x2;
  end
  else
  begin
    Result[1].x := x2;
    Result[2].x := x1;
  end;

  if y1 <= y2 then
  begin
    Result[1].y := y1;
    Result[2].y := y2;
  end
  else
  begin
    Result[1].y := y2;
    Result[2].y := y1;
  end;
end;
(* End of Equate Rectangle *)


function EquateRectangle(const Point1,Point2:TGeoPoint2D):TGeoRectangle;
begin
  Result := EquateRectangle(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of Equate Rectangle *)


procedure EquateRectangle(const x1,y1,x2,y2:TGeoFloat; out Rect:TGeoRectangle);
begin
  if x1 <= x2 then
  begin
    Rect[1].x := x1;
    Rect[2].x := x2;
  end
  else
  begin
    Rect[1].x := x2;
    Rect[2].x := x1;
  end;

  if y1 <= y2 then
  begin
    Rect[1].y := y1;
    Rect[2].y := y2;
  end
  else
  begin
    Rect[1].y := y2;
    Rect[2].y := y1;
  end;
end;
(* End of Equate Rectangle *)


procedure EquateRectangle(const Point1,Point2:TGeoPoint2D; out Rect:TGeoRectangle);
begin
  EquateRectangle(Point1.x,Point1.y,Point2.x,Point2.y,Rect);
end;
(* End of Equate Rectangle *)


function EquateTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoTriangle2D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[2].x := x2;
  Result[2].y := y2;
  Result[3].x := x3;
  Result[3].y := y3;
end;
(* End of Equate Triangle *)


function EquateTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):TGeoTriangle3D;
begin
  Result[1].x := x1;
  Result[2].x := x2;
  Result[3].x := x3;
  Result[1].y := y1;
  Result[2].y := y2;
  Result[3].y := y3;
  Result[1].z := z1;
  Result[2].z := z2;
  Result[3].z := z3;
end;
(* End of Equate Triangle *)

function EquateTriangle(const Point1,Point2,Point3: TGeoPoint2D):TGeoTriangle2D;
begin
  Result[1] := Point1;
  Result[2] := Point2;
  Result[3] := Point3;
end;
(* End of Equate Triangle *)


function EquateTriangle(const Point1,Point2,Point3: TGeoPoint3D):TGeoTriangle3D;
begin
 Result[1] := Point1;
 Result[2] := Point2;
 Result[3] := Point3;
end;
(* End of Equate Triangle *)


procedure EquateTriangle(const x1,y1,x2,y2,x3,y3:TGeoFloat; out Triangle:TGeoTriangle2D);
begin
 Triangle[1].x := x1;
 Triangle[2].x := x2;
 Triangle[3].x := x3;
 Triangle[1].y := y1;
 Triangle[2].y := y2;
 Triangle[3].y := y3;
end;
(* End of Equate Triangle *)


procedure EquateTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat; out Triangle:TGeoTriangle3D);
begin
  Triangle[1].x := x1;
  Triangle[2].x := x2;
  Triangle[3].x := x3;
  Triangle[1].y := y1;
  Triangle[2].y := y2;
  Triangle[3].y := y3;
  Triangle[1].z := z1;
  Triangle[2].z := z2;
  Triangle[3].z := z3;
end;
(* End of Equate Triangle *)


procedure EquateTriangle(const Point1,Point2,Point3:TGeoPoint2D; out Triangle:TGeoTriangle2D);
begin
  Triangle[1] := Point1;
  Triangle[2] := Point2;
  Triangle[3] := Point3;
end;
(* End of Equate Triangle *)


procedure EquateTriangle(const Point1,Point2,Point3:TGeoPoint3D; out Triangle:TGeoTriangle3D);
begin
  Triangle[1] := Point1;
  Triangle[2] := Point2;
  Triangle[3] := Point3;
end;
(* End of Equate Triangle *)


function EquateCircle(const x,y,r:TGeoFloat):TGeoCircle;
begin
  Result.x      := x;
  Result.y      := y;
  Result.Radius := r;
end;
(* End of Equate Circle *)


function EquateCircle(const Point:TGeoPoint2D; Radius:TGeoFloat):TGeoCircle;
begin
  Result := EquateCircle(Point.x,Point.y,Radius);
end;
(* End of Equate Circle *)


procedure EquateCircle(const x,y,r:TGeoFloat; out Circle:TGeoCircle);
begin
  Circle.x      := x;
  Circle.y      := y;
  Circle.Radius := r;
end;
(* End of Equate Circle *)


function EquateSphere(const x,y,z,r:TGeoFloat):TGeoSphere;
begin
  Result.x      := x;
  Result.y      := y;
  Result.z      := z;
  Result.Radius := r;
end;
(* End of Equate Sphere *)


procedure EquateSphere(const x,y,z,r:TGeoFloat; out Sphere:TGeoSphere);
begin
  Sphere.x      := x;
  Sphere.y      := y;
  Sphere.z      := z;
  Sphere.Radius := r;
end;
(* End of Equate Sphere *)


function EquatePlane(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat):TGeoPlane2D;
begin
  Result.a := y1 * (z2 - z3) + y2 * (z3 - z1) + y3 * (z1 - z2);
  Result.b := z1 * (x2 - x3) + z2 * (x3 - x1) + z3 * (x1 - x2);
  Result.c := x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2);
  Result.d := -(x1 * (y2 * z3 - y3 * z2) + x2 * (y3 * z1 - y1 * z3) + x3 * (y1 * z2 - y2 * z1));
end;
(* End of Equate Plane *)


function EquatePlane(const Point1,Point2,Point3:TGeoPoint3D):TGeoPlane2D;
begin
  Result := EquatePlane(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z);
end;
(* End of Equate Plane *)


procedure EquatePlane(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat; out Plane:TGeoPlane2D);
begin
  Plane := EquatePlane(x1,y1,z1,x2,y2,z2,x3,y3,z3);
end;
(* End of Equate Plane *)


procedure EquatePlane(const Point1,Point2,Point3:TGeoPoint3D; out Plane:TGeoPlane2D);
begin
  EquatePlane(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Plane);
end;
(* End of Equate Plane *)


procedure EquateBezier(const x1,y1,x2,y2,x3,y3:TGeoFloat; out Bezier:TGeoQuadraticBezier2D);
begin
  Bezier[0].x := x1;
  Bezier[0].y := y1;
  Bezier[1].x := x2;
  Bezier[1].y := y2;
  Bezier[2].x := x3;
  Bezier[2].y := y3;
end;
(* End of Equate Bezier *)


procedure EquateBezier(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TGeoFloat; out Bezier:TGeoQuadraticBezier3D);
begin
  Bezier[0].x := x1;
  Bezier[0].y := y1;
  Bezier[0].z := z1;
  Bezier[1].x := x2;
  Bezier[1].y := y2;
  Bezier[1].z := z2;
  Bezier[2].x := x3;
  Bezier[2].y := y3;
  Bezier[2].z := z3;
end;
(* End of Equate Bezier *)


function EquateBezier(const Pnt1,Pnt2,Pnt3:TGeoPoint2D):TGeoQuadraticBezier2D;
begin
  Result[0] := Pnt1;
  Result[1] := Pnt2;
  Result[2] := Pnt3;
end;
(* End of Equate Bezier *)


function EquateBezier(const Pnt1,Pnt2,Pnt3:TGeoPoint3D):TGeoQuadraticBezier3D;
begin
  Result[0] := Pnt1;
  Result[1] := Pnt2;
  Result[2] := Pnt3;
end;
(* End of Equate Bezier *)


procedure EquateBezier(const x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat; out Bezier:TGeoCubicBezier2D);
begin
  Bezier[0].x := x1;
  Bezier[0].y := y1;
  Bezier[1].x := x2;
  Bezier[1].y := y2;
  Bezier[2].x := x3;
  Bezier[2].y := y3;
  Bezier[3].x := x4;
  Bezier[3].y := y4;
end;
(* End of Equate Bezier *)


procedure EquateBezier(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TGeoFloat; out Bezier:TGeoCubicBezier3D);
begin
  Bezier[0].x := x1;
  Bezier[0].y := y1;
  Bezier[0].z := z1;
  Bezier[1].x := x2;
  Bezier[1].y := y2;
  Bezier[1].z := z2;
  Bezier[2].x := x3;
  Bezier[2].y := y3;
  Bezier[2].z := z3;
  Bezier[3].x := x4;
  Bezier[3].y := y4;
  Bezier[3].z := z4;
end;
(* End of Equate Bezier *)


function EquateBezier(const Pnt1,Pnt2,Pnt3,Pnt4:TGeoPoint2D):TGeoCubicBezier2D;
begin
  Result[0] := Pnt1;
  Result[1] := Pnt2;
  Result[2] := Pnt3;
  Result[3] := Pnt4;
end;
(* End of Equate Bezier *)


function EquateBezier(const Pnt1,Pnt2,Pnt3,Pnt4:TGeoPoint3D):TGeoCubicBezier3D;
begin
  Result[0] := Pnt1;
  Result[1] := Pnt2;
  Result[2] := Pnt3;
  Result[3] := Pnt4;
end;
(* End of Equate Bezier *)


function RectangleToQuadix(x1,y1,x2,y2:TGeoFloat):TQuadix2D;
begin
  Result[1] := EquatePoint(x1,y1);
  Result[2] := EquatePoint(x2,y1);
  Result[3] := EquatePoint(x2,y2);
  Result[4] := EquatePoint(x1,y2);
end;
(* End of RectangleToQuadix *)


function RectangleToQuadix(Point1,Point2:TGeoPoint2D):TQuadix2D;
begin
  Result := RectangleToQuadix(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of RectangleToQuadix *)


function RectangleToQuadix(Rectangle:TGeoRectangle):TQuadix2D;
begin
  Result := RectangleToQuadix(Rectangle[1],Rectangle[2]);
end;
(* End of RectangleToQuadix *)


function TriangleToPolygon(x1,y1,x2,y2,x3,y3:TGeoFloat):TGeoPolygon2D;
begin
  SetLength(Result,3);
  Result[0] := EquatePoint(x1,y1);
  Result[1] := EquatePoint(x2,y2);
  Result[2] := EquatePoint(x3,y3);
end;
(* End of TriangleToPolygon *)


function TriangleToPolygon(Triangle:TGeoTriangle2D):TGeoPolygon2D;
begin
  Result := TriangleToPolygon(Triangle[1].x,Triangle[1].y,
                              Triangle[2].x,Triangle[2].y,
                              Triangle[3].x,Triangle[3].y);
end;
(* End of TriangleToPolygon *)


function QuadixToPolygon(x1,y1,x2,y2,x3,y3,x4,y4:TGeoFloat):TGeoPolygon2D;
begin
  SetLength(Result,4);
  Result[0] := EquatePoint(x1,y1);
  Result[1] := EquatePoint(x2,y2);
  Result[2] := EquatePoint(x3,y3);
  Result[3] := EquatePoint(x4,y4);
end;
(* QuadixToPolygon *)


function QuadixToPolygon(Quadix:TQuadix2D):TGeoPolygon2D;
begin
  Result := QuadixToPolygon(Quadix[1].x,Quadix[1].y,
                            Quadix[2].x,Quadix[2].y,
                            Quadix[3].x,Quadix[3].y,
                            Quadix[4].x,Quadix[4].y);
end;
(* End of QuadixToPolygon *)


function CircleToPolygon(const Cx,Cy,Radius:TGeoFloat; const PointCount:Integer):TGeoPolygon2D;
var
  i     : Integer;
  Angle : TGeoFloat;
begin
  SetLength(Result,PointCount);
  Angle := 360.0 / (1.0 * PointCount);
  for i := 0 to PointCount - 1 do
  begin
    Rotate(Angle * i,Cx + Radius, Cy,Cx,Cy,Result[i].x,Result[i].y);
  end;
end;
(* End of Circle To Polygon *)


function CircleToPolygon(const Circle:TGeoCircle; const PointCount:Integer):TGeoPolygon2D;
begin
  Result := CircleToPolygon(Circle.x,Circle.y,Circle.Radius,PointCount);
end;
(* End of Circle To Polygon *)


procedure SetGeometricObject(const Primitive:TGeoPoint2D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoPoint2D;
  GeoObj.Point2D    := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TGeoPoint3D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoPoint3D;
  GeoObj.Point3D    := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TGeoLine2D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoLine2D;
  GeoObj.Line2D     := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TGeoLine3D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoLine3D;
  GeoObj.Line3D     := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TGeoSegment2D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoSegment2D;
  GeoObj.Segment2D  := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TGeoSegment3D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoSegment3D;
  GeoObj.Segment3D  := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TGeoTriangle2D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoTriangle2D;
  GeoObj.Triangle2D := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TGeoTriangle3D; out GeoObj:TGeometricObject);
begin
 GeoObj.ObjectType := geoTriangle3D;
 GeoObj.Triangle3D := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TQuadix2D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoQuadix2D;
  GeoObj.Quadix2D   := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TQuadix3D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoQuadix3D;
  GeoObj.Quadix3D   := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TGeoRectangle; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoRectangle;
  GeoObj.Rectangle  := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TGeoCircle; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoCircle;
  GeoObj.Circle     := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TGeoSphere; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoSphere;
  GeoObj.Sphere     := Primitive;
end;
(* End of Set Geometric Object *)


function GenerateRandomValue(Range : TGeoFloat; Resolution : TGeoFloat = 10000000.0) : TGeoFloat;
begin
  (* { Result e R : 0 <= Result < Range } *)
  if Range > 1.0 then
    Result := (1.0 * Random(Round(Range) - 1)) + ((1.0 * Random(Round(Resolution))) / Resolution)
  else
    Result := ((1.0 * Random(Round(Resolution))) / Resolution);
end;
(* End of Generate Random Value *)


procedure GenerateRandomPoints(const Bx1,By1,Bx2,By2:TGeoFloat; var PointList: TPointList2D); overload;
var
  i  : Integer;
  Dx : TGeoFloat;
  Dy : TGeoFloat;
begin
  Randomize;
  Dx  := Abs(Bx2 - Bx1);
  Dy  := Abs(By2 - By1);
  for i := 0 to Length(PointList) - 1 do
  begin
    PointList[i].x := Bx1 + GenerateRandomValue(Dx);
    PointList[i].y := By1 + GenerateRandomValue(Dy);
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Rectangle:TGeoRectangle; var PointList: TPointList2D);
begin
  GenerateRandomPoints(Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y,PointList);
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Segment:TGeoSegment2D; var PointList: TPointList2D);
var
  i     : Integer;
  Ratio : TGeoFloat;
begin
  for i := 0 to Length(PointList) - 1 do
  begin
    Ratio := GenerateRandomValue(1.0);
    PointList[i].x := ((1 - Ratio) * Segment[1].x) + (Ratio * Segment[2].x);
    PointList[i].y := ((1 - Ratio) * Segment[1].y) + (Ratio * Segment[2].y);
  end;
end;
(* End of Generate Random Points *)

procedure GenerateRandomPoints(const Segment:TGeoSegment3D; var PointList: TPointList3D);
var
  i     : Integer;
  Ratio : TGeoFloat;
begin
  for i := 0 to Length(PointList) - 1 do
  begin
    Ratio := GenerateRandomValue(1.0);
    PointList[i].x := ((1 - Ratio) * Segment[1].x) + (Ratio * Segment[2].x);
    PointList[i].y := ((1 - Ratio) * Segment[1].y) + (Ratio * Segment[2].y);
    PointList[i].z := ((1 - Ratio) * Segment[1].z) + (Ratio * Segment[2].z);
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Triangle:TGeoTriangle2D; var PointList: TPointList2D);
var
  a : TGeoFloat;
  b : TGeoFloat;
  c : TGeoFloat;
  i : Integer;
begin
  for i := 0 to Length(PointList) - 1 do
  begin
    a := RandomValue;
    b := RandomValue;
    if (a + b) > 1 then
    begin
      a := 1 - a;
      b := 1 - b;
    end;
    c := (1 - a - b);
    PointList[i].x := (Triangle[1].x * a) + (Triangle[2].x * b) + (Triangle[3].x * c);
    PointList[i].y := (Triangle[1].y * a) + (Triangle[2].y * b) + (Triangle[3].y * c);
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Triangle:TGeoTriangle3D; var PointList: TPointList3D);
var
  a : TGeoFloat;
  b : TGeoFloat;
  c : TGeoFloat;
  i : Integer;
begin
  for i := 0 to Length(PointList) - 1 do
  begin
    a := RandomValue;
    b := RandomValue;
    if (a + b) > 1 then
    begin
      a := 1 - a;
      b := 1 - b;
    end;
    c := (1 - a - b);
    PointList[i].x := (Triangle[1].x * a) + (Triangle[2].x * b) + (Triangle[3].x * c);
    PointList[i].y := (Triangle[1].y * a) + (Triangle[2].y * b) + (Triangle[3].y * c);
    PointList[i].z := (Triangle[1].z * a) + (Triangle[2].z * b) + (Triangle[3].z * c);
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Circle:TGeoCircle; var PointList: TPointList2D);
var
  i           : Integer;
  RandomAngle : TGeoFloat;
  CPoint      : TGeoPoint2D;
begin
  CPoint := EquatePoint(Circle.x, Circle.y);
  for i := 0 to Length(PointList) - 1 do
  begin
    RandomAngle := GenerateRandomValue(360.0);
    PointList[i].x  := Circle.x + Circle.Radius * Sqrt(RandomValue);
    PointList[i].y  := Circle.y;
    PointList[i]    := Rotate(RandomAngle, PointList[i], CPoint);
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Quadix:TQuadix2D; var PointList : TPointList2D);
(*  Note: It is assumed the input Quadix is convex. *)
var
  i  : Integer;
  a  : TGeoFloat;
  b  : TGeoFloat;
  a1 : TGeoFloat;
  b1 : TGeoFloat;
  a2 : TGeoFloat;
  b2 : TGeoFloat;
  r1 : TGeoFloat;
  r2 : TGeoFloat;
  r3 : TGeoFloat;
  r4 : TGeoFloat;
begin
  for i := 0 to Length(PointList) - 1 do
  begin
    a := (2 * RandomValue) - 1;
    b := (2 * RandomValue) - 1;

    a1 := 1 - a;
    a2 := 1 + a;

    b1 := 1 - b;
    b2 := 1 + b;

    r1 := a1 * b1;
    r2 := a2 * b1;
    r3 := a2 * b2;
    r4 := a1 * b2;

    PointList[i].x := ((r1 * Quadix[1].x) + (r2 * Quadix[2].x) + (r3 * Quadix[3].x) + (r4 * Quadix[4].x)) * 0.25;
    PointList[i].y := ((r1 * Quadix[1].y) + (r2 * Quadix[2].y) + (r3 * Quadix[3].y) + (r4 * Quadix[4].y)) * 0.25;
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Quadix:TQuadix3D; var PointList : TPointList3D);
(*  Note: It is assumed the input Quadix is convex. *)
var
  i  : Integer;
  a  : TGeoFloat;
  b  : TGeoFloat;
  a1 : TGeoFloat;
  b1 : TGeoFloat;
  a2 : TGeoFloat;
  b2 : TGeoFloat;
  r1 : TGeoFloat;
  r2 : TGeoFloat;
  r3 : TGeoFloat;
  r4 : TGeoFloat;
begin
  for i := 0 to Length(PointList) - 1 do
  begin
    a := (2 * RandomValue) - 1;
    b := (2 * RandomValue) - 1;

    a1 := 1 - a;
    a2 := 1 + a;

    b1 := 1 - b;
    b2 := 1 + b;

    r1 := a1 * b1;
    r2 := a2 * b1;
    r3 := a2 * b2;
    r4 := a1 * b2;

    PointList[i].x := ((r1 * Quadix[1].x) + (r2 * Quadix[2].x) + (r3 * Quadix[3].x) + (r4 * Quadix[4].x)) * 0.25;
    PointList[i].y := ((r1 * Quadix[1].y) + (r2 * Quadix[2].y) + (r3 * Quadix[3].y) + (r4 * Quadix[4].y)) * 0.25;
    PointList[i].z := ((r1 * Quadix[1].z) + (r2 * Quadix[2].z) + (r3 * Quadix[3].z) + (r4 * Quadix[4].z)) * 0.25;
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPointsOnConvexPentagon(const Pentagon : TGeoPolygon2D; var PointList : TPointList2D);
var
  i            : Integer;
  Triangle     : TGeoTriangle2D;
  Quadix       : TQuadix2D;
  QRndPoint    : TPointList2D;
  TRndPoint    : TPointList2D;
  QRndPntCnt   : Integer;
  TRndPntCnt   : Integer;
begin
  if Length(Pentagon) <> 5       then Exit;
  if not ConvexPolygon(Pentagon) then Exit;
  Quadix     := EquateQuadix(Pentagon[0],Pentagon[1],Pentagon[2],Pentagon[3]);
  Triangle   := EquateTriangle(Pentagon[3],Pentagon[4],Pentagon[0]);
  QRndPntCnt := Round(1.0 * Length(PointList) * (Area(Quadix) / Area(Pentagon)));
  TRndPntCnt := Length(PointList) - QRndPntCnt;
  SetLength(QRndPoint,QRndPntCnt);
  SetLength(TRndPoint,TRndPntCnt);
  GenerateRandomPoints(Quadix,QRndPoint);
  GenerateRandomPoints(Triangle,TRndPoint);
  for i := 0 to Length(QRndPoint) - 1 do PointList[i                    ] := QRndPoint[i];
  for i := 0 to Length(TRndPoint) - 1 do PointList[i + Length(QRndPoint)] := TRndPoint[i];
  finalize(QRndPoint);
  finalize(TRndPoint);
end;
(* End of Generate Random Points On Convex Pentagon *)


procedure GenerateRandomPointsOnConvexHexagon(const Hexagon : TGeoPolygon2D; var PointList : TPointList2D);
var
  i           : Integer;
  Quadix1     : TQuadix2D;
  Quadix2     : TQuadix2D;
  Q1RndPoint  : TPointList2D;
  Q2RndPoint  : TPointList2D;
  Q1RndPntCnt : Integer;
  Q2RndPntCnt : Integer;
begin
  if Length(Hexagon) <> 6       then Exit;
  if not ConvexPolygon(Hexagon) then Exit;
  Quadix1      := EquateQuadix(Hexagon[0],Hexagon[1],Hexagon[2],Hexagon[5]);
  Quadix2      := EquateQuadix(Hexagon[2],Hexagon[3],Hexagon[4],Hexagon[5]);
  Q1RndPntCnt  := Round(1.0 * Length(PointList) * (Area(Quadix1) / Area(Hexagon)));
  Q2RndPntCnt  := Length(PointList) - Q1RndPntCnt;
  SetLength(Q1RndPoint,Q1RndPntCnt);
  SetLength(Q2RndPoint,Q2RndPntCnt);
  GenerateRandomPoints(Quadix1,Q1RndPoint);
  GenerateRandomPoints(Quadix2,Q2RndPoint);
  for i := 0 to Length(Q1RndPoint) - 1 do PointList[i                     ] := Q1RndPoint[i];
  for i := 0 to Length(Q2RndPoint) - 1 do PointList[i + Length(Q1RndPoint)] := Q2RndPoint[i];
  finalize(Q1RndPoint);
  finalize(Q2RndPoint);
end;
(* End of Generate Random Points On Convex Hexagon *)


procedure GenerateRandomPointsOnConvexHeptagon(const Heptagon : TGeoPolygon2D; var PointList : TPointList2D);
var
  i           : Integer;
  Quadix1     : TQuadix2D;
  Quadix2     : TQuadix2D;
  Triangle    : TGeoTriangle2D;
  Q1RndPoint  : TPointList2D;
  Q2RndPoint  : TPointList2D;
  TRndPoint   : TPointList2D;
  Q1RndPntCnt : Integer;
  Q2RndPntCnt : Integer;
  TRndPntCnt  : Integer;
begin
  if Length(Heptagon) <> 7       then Exit;
  if not ConvexPolygon(Heptagon) then Exit;
  Quadix1     := EquateQuadix(Heptagon[0],Heptagon[1],Heptagon[2],Heptagon[3]);
  Quadix2     := EquateQuadix(Heptagon[3],Heptagon[4],Heptagon[5],Heptagon[6]);
  Triangle    := EquateTriangle(Heptagon[6],Heptagon[0],Heptagon[3]);
  Q1RndPntCnt := Round(1.0 * Length(PointList) * (Area(Quadix1) / Area(Heptagon)));
  Q2RndPntCnt := Round(1.0 * (Length(PointList) - Q1RndPntCnt) * (Area(Quadix2) / (Area(Quadix2) + Area(Triangle))));
  TRndPntCnt  := Length(PointList) - Q1RndPntCnt - Q2RndPntCnt;
  SetLength(Q1RndPoint,Q1RndPntCnt);
  SetLength(Q2RndPoint,Q2RndPntCnt);
  SetLength(TRndPoint ,TRndPntCnt );
  GenerateRandomPoints(Quadix1,Q1RndPoint);
  GenerateRandomPoints(Quadix2,Q2RndPoint);
  GenerateRandomPoints(Triangle,TRndPoint);
  for i := 0 to Length(Q1RndPoint) - 1 do PointList[i                                          ] := Q1RndPoint[i];
  for i := 0 to Length(Q2RndPoint) - 1 do PointList[i + Length(Q1RndPoint)                     ] := Q2RndPoint[i];
  for i := 0 to Length(TRndPoint ) - 1 do PointList[i + Length(Q1RndPoint) + Length(Q2RndPoint)] := TRndPoint [i];
  finalize(Q1RndPoint);
  finalize(Q2RndPoint);
  finalize(TRndPoint);
end;
(* End of Generate Random Points On Convex Heptagon *)


procedure GenerateRandomPointsOnConvexOctagon(const Octagon : TGeoPolygon2D; var PointList : TPointList2D);
var
  i           : Integer;
  Quadix1     : TQuadix2D;
  Quadix2     : TQuadix2D;
  Quadix3     : TQuadix2D;
  Q1RndPoint  : TPointList2D;
  Q2RndPoint  : TPointList2D;
  Q3RndPoint  : TPointList2D;
  Q1RndPntCnt : Integer;
  Q2RndPntCnt : Integer;
  Q3RndPntCnt : Integer;
begin
  if Length(Octagon) <> 8       then Exit;
  if not ConvexPolygon(Octagon) then Exit;
  Quadix1     := EquateQuadix(Octagon[0],Octagon[1],Octagon[2],Octagon[3]);
  Quadix2     := EquateQuadix(Octagon[3],Octagon[4],Octagon[5],Octagon[6]);
  Quadix3     := EquateQuadix(Octagon[0],Octagon[3],Octagon[6],Octagon[7]);
  Q1RndPntCnt := Round(1.0 * Length(PointList) * (Area(Quadix1) / Area(Octagon)));
  Q2RndPntCnt := Round(1.0 * (Length(PointList) - Q1RndPntCnt) * (Area(Quadix2) / (Area(Quadix2) + Area(Quadix3))));
  Q3RndPntCnt := Length(PointList) - Q1RndPntCnt - Q2RndPntCnt;
  SetLength(Q1RndPoint,Q1RndPntCnt);
  SetLength(Q2RndPoint,Q2RndPntCnt);
  SetLength(Q3RndPoint,Q3RndPntCnt);
  GenerateRandomPoints(Quadix1,Q1RndPoint);
  GenerateRandomPoints(Quadix2,Q2RndPoint);
  GenerateRandomPoints(Quadix3,Q3RndPoint);
  for i := 0 to Length(Q1RndPoint) - 1 do PointList[i                                          ] := Q1RndPoint[i];
  for i := 0 to Length(Q2RndPoint) - 1 do PointList[i + Length(Q1RndPoint)                     ] := Q2RndPoint[i];
  for i := 0 to Length(Q2RndPoint) - 1 do PointList[i + Length(Q1RndPoint) + Length(Q2RndPoint)] := Q3RndPoint[i];
  finalize(Q1RndPoint);
  finalize(Q2RndPoint);
  finalize(Q3RndPoint);
end;
(* End of Generate Random Points On Convex Octagon *)


procedure GenerateRandomTriangle(const Bx1,By1,Bx2,By2:TGeoFloat; out Triangle : TGeoTriangle2D);
var
  Dx : TGeoFloat;
  Dy : TGeoFloat;
begin
  Dx := Abs(Bx2 - Bx1);
  Dy := Abs(By2 - By1);
  repeat
    Triangle[1].x := Bx1 + GenerateRandomValue(Dx);
    Triangle[1].y := By1 + GenerateRandomValue(Dy);

    Triangle[2].x := Bx1 + GenerateRandomValue(Dx);
    Triangle[2].y := By1 + GenerateRandomValue(Dy);

    Triangle[3].x := Bx1 + GenerateRandomValue(Dx);
    Triangle[3].y := By1 + GenerateRandomValue(Dy);
  until not IsDegenerate(Triangle);
end;
(* End of Generate Random Triangle *)


procedure GenerateRandomQuadix(const Bx1,By1,Bx2,By2:TGeoFloat; out Quadix : TQuadix2D);
var
  Dx : Integer;
  Dy : Integer;
begin
  Dx := Round(Abs(Bx2 - Bx1) - 1.0);
  Dy := Round(Abs(By2 - By1) - 1.0);
  repeat
    Quadix[1].x := Bx1 + Random(Dx) + RandomValue;
    Quadix[1].y := By1 + Random(Dy) + RandomValue;

    Quadix[2].x := Bx1 + Random(Dx) + RandomValue;
    Quadix[2].y := By1 + Random(Dy) + RandomValue;

    Quadix[3].x := Bx1 + Random(Dx) + RandomValue;
    Quadix[3].y := By1 + Random(Dy) + RandomValue;

    Quadix[4].x := Bx1 + Random(Dx) + RandomValue;
    Quadix[4].y := By1 + Random(Dy) + RandomValue;
  until not IsDegenerate(Quadix);
end;
(* End of Generate Random Quadix *)


procedure GenerateRandomCircle(const Bx1,By1,Bx2,By2:TGeoFloat; out Circle : TGeoCircle);
var
  Dx : TGeoFloat;
  Dy : TGeoFloat;
begin
  Dx := Abs(Bx2 - Bx1) - 1.0;
  Dy := Abs(By2 - By1) - 1.0;
  Circle.Radius := Random(Round(Min(Dx,Dy) * 0.5)) + RandomValue;
  Circle.x      := Bx1 + Circle.Radius + Random(Round(Dx - (2.0 * Circle.Radius))) + RandomValue;
  Circle.y      := By1 + Circle.Radius + Random(Round(Dy - (2.0 * Circle.Radius))) + RandomValue;
end;
(* End of Generate Random Circle *)


function Add(const Vec1,Vec2:TGeoVector2D):TGeoVector2D;
begin
  Result.x := Vec1.x + Vec2.x;
  Result.y := Vec1.y + Vec2.y;
end;
(* End of Add *)


function Add(const Vec1,Vec2:TGeoVector3D):TGeoVector3D;
begin
  Result.x := Vec1.x + Vec2.x;
  Result.y := Vec1.y + Vec2.y;
  Result.z := Vec1.z + Vec2.z;
end;
(* End of Add *)


function Add(const Vec:TGeoVector2DArray):TGeoVector2D;
var
  i : Integer;
begin
  Result.x := Zero;
  Result.y := Zero;
  for i := 0 to Length(Vec) - 1 do
  begin
    Result.x := Result.x + Vec[i].x;
    Result.y := Result.y + Vec[i].y;
  end;
end;
(* End of Add *)


function Add(const Vec:TGeoVector3DArray):TGeoVector3D;
var
  i : Integer;
begin
  Result.x := Zero;
  Result.y := Zero;
  Result.z := Zero;
  for i := 0 to Length(Vec) - 1 do
  begin
    Result.x := Result.x + Vec[i].x;
    Result.y := Result.y + Vec[i].y;
    Result.z := Result.z + Vec[i].z;
  end;
end;
(* End of Add *)


function Add(const Vec1,Vec2:TGeoVector2DArray):TGeoVector2DArray;
var
  i : Integer;
begin
  SetLength(Result,Min(Length(Vec1),Length(Vec2)));
  for i := 0 to Min(Length(Vec1),Length(Vec2)) - 1 do
  begin
    Result[i] := Add(Vec1[i],Vec2[i]);
  end;
end;
(* End of Add *)


function Add(const Vec1,Vec2:TGeoVector3DArray):TGeoVector3DArray;
var
  i : Integer;
begin
  SetLength(Result,Min(Length(Vec1),Length(Vec2)));
  for i := 0 to Min(Length(Vec1),Length(Vec2)) - 1 do
  begin
    Result[i] := Add(Vec1[i],Vec2[i]);
  end;
end;
(* End of Add *)


function Sub(const Vec1,Vec2:TGeoVector2D):TGeoVector2D;
begin
  Result.x := Vec1.x - Vec2.x;
  Result.y := Vec1.y - Vec2.y;
end;
(* End of Sub *)


function Sub(const Vec1,Vec2:TGeoVector3D):TGeoVector3D;
begin
  Result.x := Vec1.x - Vec2.x;
  Result.y := Vec1.y - Vec2.y;
  Result.z := Vec1.z - Vec2.z;
end;
(* End of Sub *)


function Sub(const Vec1,Vec2:TGeoVector2DArray):TGeoVector2DArray;
var
  i : Integer;
begin
  SetLength(Result,Min(Length(Vec1),Length(Vec2)));
  for i := 0 to Min(Length(Vec1),Length(Vec2)) - 1 do
  begin
    Result[i] := Sub(Vec1[i],Vec2[i]);
  end;
end;
(* End of Sub *)


function Sub(const Vec1,Vec2:TGeoVector3DArray):TGeoVector3DArray;
var
  i : Integer;
begin
  SetLength(Result,Min(Length(Vec1),Length(Vec2)));
  for i := 0 to Min(Length(Vec1),Length(Vec2)) - 1 do
  begin
    Result[i] := Sub(Vec1[i],Vec2[i]);
  end;
end;
(* End of Sub *)


function Mul(const Vec1,Vec2:TGeoVector2D):TGeoVector3D;
begin
end;
(* End of *)


function Mul(const Vec1,Vec2:TGeoVector3D):TGeoVector3D;
begin
  Result.x := Vec1.y * Vec2.z - Vec1.z * Vec2.y;
  Result.y := Vec1.z * Vec2.x - Vec1.x * Vec2.z;
  Result.z := Vec1.x * Vec2.y - Vec1.y * Vec2.x;
end;
(* End of Multiply (cross-product) *)


function Mul(const Vec1,Vec2:TGeoVector3DArray):TGeoVector3DArray;
var
  i : Integer;
begin
  SetLength(Result,Min(Length(Vec1),Length(Vec2)));
  for i := 0 to Min(Length(Vec1),Length(Vec2)) - 1 do
  begin
    Result[i] := Mul(Vec1[i],Vec2[i]);
  end;
end;
(* End of Multiply (cross-product) *)


function UniTGeoVector(const Vec:TGeoVector2D):TGeoVector2D;
var
  Mag : TGeoFloat;
begin
  Mag := Magnitude(Vec);
  if Mag > Zero then
  begin
    Result.x := Vec.x / Mag;
    Result.y := Vec.y / Mag;
  end
  else
  begin
    Result.x := Zero;
    Result.y := Zero;
  end;
end;
(* End of UniTGeoVector *)


function UniTGeoVector(const Vec:TGeoVector3D):TGeoVector3D;
var
  Mag : TGeoFloat;
begin
  Mag := Magnitude(Vec);
  if Mag > Zero then
  begin
    Result.x := Vec.x / Mag;
    Result.y := Vec.y / Mag;
    Result.z := Vec.z / Mag;
  end
  else
  begin
    Result.x := Zero;
    Result.y := Zero;
    Result.z := Zero;
  end;
end;
(* End of UniTGeoVector *)


function Magnitude(const Vec:TGeoVector2D):TGeoFloat;
begin
  Result := Sqrt((Vec.x * Vec.x) + (Vec.y * Vec.y));
end;
(* End of Magnitude *)


function Magnitude(const Vec:TGeoVector3D):TGeoFloat;
begin
  Result := Sqrt((Vec.x * Vec.x) + (Vec.y * Vec.y) + (Vec.z * Vec.z));
end;
(* End of Magnitude *)


function DotProduct(const Vec1,Vec2:TGeoVector2D):TGeoFloat;
begin
  Result := Vec1.x * Vec2.x + Vec1.y * Vec2.y;
end;
(* End of dotProduct *)


function DotProduct(const Vec1,Vec2:TGeoVector3D):TGeoFloat;
begin
  Result := Vec1.x * Vec2.x + Vec1.y * Vec2.y + Vec1.z * Vec2.z;
end;
(* End of dotProduct *)


function Scale(const Vec:TGeoVector2D; const Factor:TGeoFloat):TGeoVector2D;
begin
  Result.x := Vec.x * Factor;
  Result.y := Vec.y * Factor;
end;
(* End of Scale *)


function Scale(const Vec:TGeoVector3D; const Factor:TGeoFloat):TGeoVector3D;
begin
  Result.x := Vec.x * Factor;
  Result.y := Vec.y * Factor;
  Result.z := Vec.z * Factor;
end;
(* End of Scale *)


function Scale(const Vec:TGeoVector2DArray; const Factor:TGeoFloat):TGeoVector2DArray;
var
  i : Integer;
begin
  SetLength(Result,Length(Vec));
  for i := 0 to Length(Vec) - 1 do
  begin
    Result[i].x := Vec[i].x * Factor;
    Result[i].y := Vec[i].y * Factor;
  end;
end;
(* End of Scale *)


function Scale(const Vec:TGeoVector3DArray; const Factor:TGeoFloat):TGeoVector3DArray;
var
  i : Integer;
begin
  SetLength(Result,Length(Vec));
  for i := 0 to Length(Vec) - 1 do
  begin
    Result[i].x := Vec[i].x * Factor;
    Result[i].y := Vec[i].y * Factor;
    Result[i].z := Vec[i].z * Factor;
  end;
end;
(* End of Scale *)


function Negate(const Vec:TGeoVector2D):TGeoVector2D;
begin
  Result.x := -Vec.x;
  Result.y := -Vec.y;
end;
(* End of Negate *)


function Negate(const Vec:TGeoVector3D):TGeoVector3D;
begin
  Result.x := -Vec.x;
  Result.y := -Vec.y;
  Result.z := -Vec.z;
end;
(* End of Negate *)


function Negate(Vec:TGeoVector2DArray):TGeoVector2DArray;
var
  i : Integer;
begin
  SetLength(Result,Length(Vec));
  for i := 0 to Length(Vec) - 1 do
  begin
    Result[i].x := -Vec[i].x;
    Result[i].y := -Vec[i].y;
  end;
end;
(* End of Negate *)


function Negate(Vec:TGeoVector3DArray):TGeoVector3DArray;
var
  i : Integer;
begin
  SetLength(Result,Length(Vec));
  for i := 0 to Length(Vec) - 1 do
  begin
    Result[i].x := -Vec[i].x;
    Result[i].y := -Vec[i].y;
    Result[i].z := -Vec[i].z;
  end;
end;
(* End of Negate *)


function RandomValue(ResInt : Integer = RandomResolutionInt; ResFlt : TGeoFloat = RandomResolutionFlt) : TGeoFloat;
begin
  Result := (1.0 * Random(ResInt)) / ResFlt;
end;
(* End of Random Value *)


procedure InitialiseTrigonometryTables;
var
  i : Integer;
begin
  (*
    Note: Trigonometry look-up tables are used to speed-up
    sine, cosine and tangent calculations.
  *)
  SetLength(CosTable,360);
  SetLength(SinTable,360);
  SetLength(TanTable,360);
  for i := 0 to 359 do
  begin
    CosTable[i] := Cos((1.0 * i) * PIDiv180);
    SinTable[i] := Sin((1.0 * i) * PIDiv180);
    TanTable[i] := Tan((1.0 * i) * PIDiv180);
  end;
end;
(* End of Initialise Trigonometry Tables *)


function IsEqual(const Val1,Val2,Epsilon:TGeoFloat):Boolean;
var
  Diff : TGeoFloat;
begin
  Diff := Val1 - Val2;
  Assert(((-Epsilon <= Diff) and (Diff <= Epsilon)) = (Abs(Diff) <= Epsilon),'Error - Illogical error in equality check. (IsEqual)');
  Result := ((-Epsilon <= Diff) and (Diff <= Epsilon));
end;
(* End of Is Equal *)


function IsEqual(const Point1,Point2:TGeoPoint2D; const Epsilon:TGeoFloat):Boolean;
begin
  Result := (IsEqual(Point1.x,Point2.x,Epsilon) and IsEqual(Point1.y,Point2.y,Epsilon));
end;
(* End of Is Equal *)


function IsEqual(const Point1,Point2:TGeoPoint3D; const Epsilon:TGeoFloat):Boolean;
begin
  Result := (IsEqual(Point1.x,Point2.x,Epsilon) and IsEqual(Point1.y,Point2.y,Epsilon) and IsEqual(Point1.z,Point2.z,Epsilon));
end;
(* End of Is Equal *)


function IsEqual(const Val1,Val2:TGeoFloat):Boolean;
begin
  Result := IsEqual(Val1,Val2,Epsilon);
end;
(* End of Is Equal *)


function IsEqual(const Point1,Point2:TGeoPoint2D):Boolean;
begin
  Result := (IsEqual(Point1.x,Point2.x,Epsilon) and IsEqual(Point1.y,Point2.y,Epsilon));
end;
(* End of Is Equal *)


function IsEqual(const Point1,Point2:TGeoPoint3D):Boolean;
begin
  Result := (IsEqual(Point1.x,Point2.x,Epsilon) and IsEqual(Point1.y,Point2.y,Epsilon) and IsEqual(Point1.z,Point2.z,Epsilon));
end;
(* End of Is Equal *)


function NotEqual(const Val1,Val2,Epsilon:TGeoFloat):Boolean;
var
  Diff : TGeoFloat;
begin
  Diff := Val1 - Val2;
  Assert(((-Epsilon > Diff) or (Diff > Epsilon)) = (Abs(Val1 - Val2) > Epsilon),'Error - Illogical error in equality check. (NotEqual)');
  Result := ((-Epsilon > Diff) or (Diff > Epsilon));
end;
(* End of not Equal *)


function NotEqual(const Point1,Point2:TGeoPoint2D; const Epsilon:TGeoFloat):Boolean;
begin
  Result := (NotEqual(Point1.x,Point2.x,Epsilon) or NotEqual(Point1.y,Point2.y,Epsilon));
end;
(* End of not Equal *)


function NotEqual(const Point1,Point2:TGeoPoint3D; const Epsilon:TGeoFloat):Boolean;
begin
  Result := (NotEqual(Point1.x,Point2.x,Epsilon) or NotEqual(Point1.y,Point2.y,Epsilon) or NotEqual(Point1.z,Point2.z,Epsilon));
end;
(* End of not Equal *)


function NotEqual(const Val1,Val2:TGeoFloat):Boolean;
begin
  Result := NotEqual(Val1,Val2,Epsilon);
end;
(* End of not Equal *)


function NotEqual(const Point1,Point2:TGeoPoint2D):Boolean;
begin
  Result := (NotEqual(Point1.x,Point2.x,Epsilon) or NotEqual(Point1.y,Point2.y,Epsilon));
end;
(* End of not Equal *)


function NotEqual(const Point1,Point2:TGeoPoint3D):Boolean;
begin
  Result := (NotEqual(Point1.x,Point2.x,Epsilon) or NotEqual(Point1.y,Point2.y,Epsilon) or NotEqual(Point1.z,Point2.z,Epsilon));
end;
(* End of not Equal *)


function LessThanOrEqual(const Val1,Val2,Epsilon:TGeoFloat):Boolean;
begin
  Result := (Val1 < Val2) or IsEqual(Val1,Val2,Epsilon);
end;
(* End of Less Than Or Equal *)


function LessThanOrEqual(const Val1,Val2:TGeoFloat):Boolean;
begin
  Result := (Val1 < Val2) or IsEqual(Val1,Val2);
end;
(* End of Less Than Or Equal *)


function GreaterThanOrEqual(const Val1,Val2,Epsilon:TGeoFloat):Boolean;
begin
  Result := (Val1 > Val2) or IsEqual(Val1,Val2,Epsilon);
end;
(* End of Less Than Or Equal *)


function GreaterThanOrEqual(const Val1,Val2:TGeoFloat):Boolean;
begin
  Result := (Val1 > Val2) or IsEqual(Val1,Val2);
end;
(* End of Less Than Or Equal *)


function IsEqualZero(const Val,Epsilon:TGeoFloat):Boolean;
begin
  Result := (abs(Val) <= Epsilon);
end;
(* End of IsEqualZero *)


function IsEqualZero(const Val:TGeoFloat):Boolean;
begin
  Result := IsEqualZero(Val,Epsilon);
end;
(* End of IsEqualZero *)


function IsDegenerate(const x1,y1,x2,y2:TGeoFloat):Boolean;
begin
  Result := IsEqual(x1,x2) and IsEqual(y1,y2);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Segment:TGeoSegment2D):Boolean;
begin
  Result := IsDegenerate(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Line:TGeoLine2D):Boolean;
begin
  Result := IsDegenerate(Line[1].x,Line[1].y,Line[2].x,Line[2].y);
end;
(* End of IsDegenerate *)


function IsDegenerate(const x1,y1,z1,x2,y2,z2:TGeoFloat):Boolean;
begin
  Result := IsEqual(x1,x2) and IsEqual(y1,y2) and IsEqual(z1,z2);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Segment:TGeoSegment3D):Boolean;
begin
  Result := IsDegenerate(Segment[1].x,Segment[1].y,Segment[1].z,Segment[2].x,Segment[2].y,Segment[2].z);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Line:TGeoLine3D):Boolean;
begin
  Result := IsDegenerate(Line[1].x,Line[1].y,Line[1].z,Line[2].x,Line[2].y,Line[2].z);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Triangle:TGeoTriangle2D):Boolean;
begin
  Result := (IsEqual(Triangle[1],Triangle[2]))                   or
            (IsEqual(Triangle[1],Triangle[3]))                   or
            (IsEqual(Triangle[2],Triangle[3]))                   or
            RobustCollinear(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Triangle:TGeoTriangle3D):Boolean;
begin
  Result := (IsEqual(Triangle[1],Triangle[2]))                   or
            (IsEqual(Triangle[1],Triangle[3]))                   or
            (IsEqual(Triangle[2],Triangle[3]))                   or
            RobustCollinear(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Quadix:TQuadix2D):Boolean;
begin
  Result :=
           (* Stage 1 unique points check *)
            IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[2].x,Quadix[2].y) or
            IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[3].x,Quadix[3].y) or
            IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[4].x,Quadix[4].y) or
            IsDegenerate(Quadix[2].x,Quadix[2].y,Quadix[3].x,Quadix[3].y) or
            IsDegenerate(Quadix[2].x,Quadix[2].y,Quadix[4].x,Quadix[4].y) or
            IsDegenerate(Quadix[3].x,Quadix[3].y,Quadix[4].x,Quadix[4].y) or
           (* Stage 2 collinearity check  *)
            Collinear(Quadix[1],Quadix[2],Quadix[3]) or
            Collinear(Quadix[2],Quadix[3],Quadix[4]) or
            Collinear(Quadix[3],Quadix[4],Quadix[1]) or
            Collinear(Quadix[4],Quadix[1],Quadix[2]) or
            Intersect(Quadix[1],Quadix[2],Quadix[3],Quadix[4]) or
            Intersect(Quadix[1],Quadix[4],Quadix[2],Quadix[3]) or
            (not ConvexQuadix(Quadix));
end;
(* End of IsDegenerate *)


function IsDegenerate(const Quadix:TQuadix3D):Boolean;
begin
  Result :=
          (* Stage 1 unique points check *)
           IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[1].z,Quadix[2].x,Quadix[2].y,Quadix[2].z) or
           IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[1].z,Quadix[3].x,Quadix[3].y,Quadix[3].z) or
           IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[1].z,Quadix[4].x,Quadix[4].y,Quadix[4].z) or
           IsDegenerate(Quadix[2].x,Quadix[2].y,Quadix[2].z,Quadix[3].x,Quadix[3].y,Quadix[3].z) or
           IsDegenerate(Quadix[2].x,Quadix[2].y,Quadix[2].z,Quadix[4].x,Quadix[4].y,Quadix[4].z) or
           IsDegenerate(Quadix[3].x,Quadix[3].y,Quadix[3].z,Quadix[4].x,Quadix[4].y,Quadix[4].z) or
          (* Stage 2 collinearity check  *)
           Collinear(Quadix[1],Quadix[2],Quadix[3]) or
           Collinear(Quadix[2],Quadix[3],Quadix[4]) or
           Collinear(Quadix[3],Quadix[4],Quadix[1]) or
           Collinear(Quadix[4],Quadix[1],Quadix[2])
end;
(* End of IsDegenerate *)


function IsDegenerate(const Rect:TGeoRectangle):Boolean;
begin
  Result := IsEqual(Rect[1],Rect[2]);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Circle:TGeoCircle):Boolean;
begin
  Result := LessThanOrEqual(Circle.Radius,Zero);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Sphere:TGeoSphere):Boolean;
begin
  Result := LessThanOrEqual(Sphere.Radius,Zero);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Arc:TGeoCircularArc2D):Boolean;
begin
  with Arc do
    Result := IsDegenerate(x1,y1,x2,y2)                                    or
              IsDegenerate(x1,y1,Cx,Cy)                                    or
              IsDegenerate(x2,y2,Cx,Cy)                                    or
              (LayDistance(x1,y1,Cx,Cy) <> LayDistance(x2,y2,Cx,Cy))       or
              (LayDistance(x1,y1,Cx,Cy) <> LayDistance(Px,Py,Cx,Cy))       or
              (CartesianAngle(x1 - Cx, y1 - Cy ) <> angle1)                or
              (CartesianAngle(x2 - Cx, y2 - Cy ) <> angle2)                or
              (CartesianAngle(Px - Cx, Py - Cy ) <> abs(angle1 - angle2))  or
              (FastGEO.Orientation (x1,y1,x2,y2,Px,Py) <> Arc.Orientation);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Obj:TGeometricObject):Boolean;
begin
  case Obj.ObjectType of
    geoSegment2D  : Result := IsDegenerate(Obj.Segment2D );
    geoSegment3D  : Result := IsDegenerate(Obj.Segment3D );
    geoLine2D     : Result := IsDegenerate(Obj.Line2D    );
    geoLine3D     : Result := IsDegenerate(Obj.Line3D    );
    geoTriangle2D : Result := IsDegenerate(Obj.Triangle2D);
    geoTriangle3D : Result := IsDegenerate(Obj.Triangle3D);
    geoQuadix2D   : Result := IsDegenerate(Obj.Quadix2D  );
    geoQuadix3D   : Result := IsDegenerate(Obj.Quadix3D  );
    geoRectangle  : Result := IsDegenerate(Obj.Rectangle );
    geoCircle     : Result := IsDegenerate(Obj.Circle    );
    geoSphere     : Result := IsDegenerate(Obj.Sphere    );
  else
    Result := False;
  end;
end;
(* End of IsDegenerate *)


procedure GeoSwap(var val1,val2:TGeoFloat);
var
  Temp : TGeoFloat;
begin
  Temp := Val1;
  Val1 := Val2;
  Val2 := Temp;
end;
(* End of Swap *)


procedure GeoSwap(var val1,val2:Integer);
var
  Temp : Integer;
begin
  Temp := Val1;
  Val1 := Val2;
  Val2 := Temp;
end;
(* End of Swap *)


procedure GeoSwap(var Point1,Point2:TGeoPoint2D);
begin
  GeoSwap(Point1.x,Point2.x);
  GeoSwap(Point1.y,Point2.y);
end;
(* End of Swap *)


procedure GeoSwap(var Point1,Point2:TGeoPoint3D);
begin
  GeoSwap(Point1.x,Point2.x);
  GeoSwap(Point1.y,Point2.y);
  GeoSwap(Point1.z,Point2.z);
end;
(* End of Swap *)


procedure GeoSwap(var Segment1,Segment2:TGeoSegment2D);
begin
  GeoSwap(Segment1[1],Segment2[1]);
  GeoSwap(Segment1[2],Segment2[2]);
end;
(* End of Swap *)


procedure GeoSwap(var Segment1,Segment2:TGeoSegment3D);
begin
  GeoSwap(Segment1[1],Segment2[1]);
  GeoSwap(Segment1[2],Segment2[2]);
end;
(* End of Swap *)


procedure GeoSwap(var Line1,Line2:TGeoLine2D);
begin
  GeoSwap(Line1[1],Line2[1]);
  GeoSwap(Line1[2],Line2[2]);
end;
(* End of Swap *)


procedure GeoSwap(var Triangle1,Triangle2:TGeoTriangle2D);
begin
  GeoSwap(Triangle1[1],Triangle2[1]);
  GeoSwap(Triangle1[2],Triangle2[2]);
  GeoSwap(Triangle1[3],Triangle2[3]);
end;
(* End of Swap *)


procedure GeoSwap(var Triangle1,Triangle2:TGeoTriangle3D);
begin
  GeoSwap(Triangle1[1],Triangle2[1]);
  GeoSwap(Triangle1[2],Triangle2[2]);
  GeoSwap(Triangle1[3],Triangle2[3]);
end;
(* End of Swap *)


procedure GeoSwap(var Quadix1,Quadix2:TQuadix2D);
begin
  GeoSwap(Quadix1[1],Quadix2[1]);
  GeoSwap(Quadix1[2],Quadix2[2]);
  GeoSwap(Quadix1[3],Quadix2[3]);
  GeoSwap(Quadix1[4],Quadix2[4]);
end;
(* End of Swap *)


procedure GeoSwap(var Quadix1,Quadix2:TQuadix3D);
begin
  GeoSwap(Quadix1[1],Quadix2[1]);
  GeoSwap(Quadix1[2],Quadix2[2]);
  GeoSwap(Quadix1[3],Quadix2[3]);
  GeoSwap(Quadix1[4],Quadix2[4]);
end;
(* End of Swap *)


procedure GeoSwap(var Circle1,Circle2:TGeoCircle);
begin
  GeoSwap(Circle1.x,Circle2.x);
  GeoSwap(Circle1.y,Circle2.y);
  GeoSwap(Circle1.Radius,Circle2.Radius);
end;
(* End of Swap *)


procedure GeoSwap(var Sphere1,Sphere2:TGeoSphere);
begin
  GeoSwap(Sphere1.x,Sphere2.x);
  GeoSwap(Sphere1.y,Sphere2.y);
  GeoSwap(Sphere1.z,Sphere2.z);
  GeoSwap(Sphere1.Radius,Sphere2.Radius);
end;
(* End of Swap *)


procedure GeoSwap(var Arc1,Arc2:TGeoCircularArc2D);
begin
  GeoSwap(Arc1.x1,Arc2.x1);
  GeoSwap(Arc1.x2,Arc2.x2);
  GeoSwap(Arc1.cx,Arc2.cx);
  GeoSwap(Arc1.px,Arc2.px);
  GeoSwap(Arc1.y1,Arc2.y1);
  GeoSwap(Arc1.y2,Arc2.y2);
  GeoSwap(Arc1.cy,Arc2.cy);
  GeoSwap(Arc1.py,Arc2.py);
  GeoSwap(Arc1.Angle1,Arc2.Angle1);
  GeoSwap(Arc1.Angle2,Arc2.Angle2);
  GeoSwap(Arc1.Orientation,Arc2.Orientation)
end;
(* End of Swap *)


function CalculateSystemEpsilon:TGeoFloat;
var
  Epsilon   : TGeoFloat;
  Check     : TGeoFloat;
  LastCheck : TGeoFloat;
begin
  Epsilon := 1.0;
  Check   := 1.0;
  repeat
    LastCheck := Check;
    Epsilon   := Epsilon * 0.5;
    Check     := 1.0 + Epsilon;
  until (Check = 1.0) or (Check = LastCheck);
  Result := Epsilon;
end;
(* End of CalculateSystemEpsilon *)


function ZeroEquivalency:Boolean;
begin
  Result := IsEqual(CalculateSystemEpsilon,Zero);
end;
(* End of ZeroEquivalency *)


function ExtendedFloatingPointTest:Boolean;
  (*
    Yet to be completed! **************************
  *)
  function RotationTest(RadiusLength : TGeoFloat) : Boolean;
  const delta_angle = 0.036;
  var
  InitialPoint : TGeoPoint2D;
  Point        : TGeoPoint2D;
  TempPoint    : TGeoPoint2D;
  i            : Integer;
  begin
    InitialPoint := EquatePoint(RadiusLength,0);
    Point        := InitialPoint;
    for i := 1 to 10000 do
    begin
      TempPoint := Rotate(delta_angle,Point);
      Point := TempPoint;
    end;
    Result := IsEqual(InitialPoint,Point);
  end;
var
  Large_Radius : TGeoFloat;
  Small_Radius : TGeoFloat;
begin
  Large_Radius := 100000.0;
  Small_Radius := 100000.0;
  Result := RotationTest(Large_Radius) And RotationTest(Small_Radius);
end;
(* End of ExtendedFloatingPointTest *)


function ExecuteTests : TNumericPrecisionResult;
begin
  Result.EEResult      := LessThanOrEqual(SystemEpsilon,Epsilon);
  Result.ZEResult      := ZeroEquivalency;
  Result.EFPResult     := ExtendedFloatingPointTest;
  Result.SystemEpsilon := SystemEpsilon;
end;
(* End of ExecuteTests *)

// calcul de l'altitude d'un point (x, y) dans un triangle
// 1. Résolution du système suivant (A, B, C: Inconnues recherchées):
// | Z1 |   | X1  Y1  1| | A |
// | Z2 | = | X2  Y2  1|.| B |
// | Z3 |   | X3  Y3  1| | C |
// 2. calcul de Z
// Z(x, y) = A.x + B.y + C
function GeoCalcAltitudeXYInTriangle(const QGeoTriangle3D: TGeoTriangle3D; const QX, QY: double): double;
begin
  Result := GeoCalcAltitudeXYInTriangle(QGeoTriangle3D[1].x, QGeoTriangle3D[1].y, QGeoTriangle3D[1].z,
                                        QGeoTriangle3D[2].x, QGeoTriangle3D[2].y, QGeoTriangle3D[2].z,
                                        QGeoTriangle3D[3].x, QGeoTriangle3D[3].y, QGeoTriangle3D[3].z,
                                        QX, QY);

end;

// calcul de l'altitude d'un point (x, y) dans un triangle
function GeoCalcAltitudeXYInTriangle(const X1, Y1, Z1,
                                           X2, Y2, Z2,
                                           X3, Y3, Z3: double;
                                     const QX, QY: double): double; overload;
var
  Det: double;
  A, B, C: double;
  Mat: array[1..3, 1..3] of Double;
begin
   // calcul du déterminant
  Det := (X2 * Y3)      +  // x2.y3 +
         (Y2 - Y3) * X1 +  // x1(y2-y3)+
        -(X3 * Y2)      +  //-x3.y2+
         (X3 - X2) * Y1;   // y1(x3-x2)+
  // matrice des coefs
  Mat[1,1] := (Y2 - Y3) / Det;
  Mat[1,2] := (Y3 - Y1) / Det;
  Mat[1,3] := (Y1 - Y2) / Det;

  Mat[2,1] := (X3 - X2) / Det;
  Mat[2,2] := (X1 - X3) / Det;
  Mat[2,3] := (X2 - X1) / Det;

  Mat[3,1] := (X2*Y3 - X3*Y2) / Det;
  Mat[3,2] := (X3*Y1 - X1*Y3) / Det;
  Mat[3,3] := (X1*Y2 - X2*Y1) / Det;
  // inconnues A, B et C
  A := Mat[1,1] * Z1 +
       Mat[1,2] * Z2 +
       Mat[1,3] * Z3 ;

  B := Mat[2,1] * Z1 +
       Mat[2,2] * Z2 +
       Mat[2,3] * Z3 ;

  C := Mat[3,1] * Z1 +
       Mat[3,2] * Z2 +
       Mat[3,3] * Z3 ;
  // calcul de l'altitude
  Result := A * QX + B*QY + C;
end;

// calcul de l'intersection entre un plan vertical passant par (X1, Y1); (X2, Y2) et un triangle 3D
// indispensable pour les profils
// valeur de retour: Le plan passe par le triangle ?
function IntersectPlanVerticalTriangle(const X1, Y1, X2, Y2: double;
                                       const QTriangle3D: TGeoTriangle3D;
                                       out IntersectP1, IntersectP2: TGeoPoint3D): boolean;
var
  Transect: TGeoSegment2D;
  Arete12,
  Arete23,
  Arete31 : TGeoSegment2D;
  Ix, Iy: TGeoFloat;
  dx, dy, dz: double;
begin
  Result := False;
  Transect[1].X := X1; Transect[1].Y := Y1;
  Transect[2].X := X2; Transect[2].Y := Y2;
  Arete12[1].x := QTriangle3D[1].x; Arete12[1].y := QTriangle3D[1].y;
  Arete12[2].x := QTriangle3D[2].x; Arete12[2].y := QTriangle3D[2].y;

  Arete23[1].x := QTriangle3D[2].x; Arete23[1].y := QTriangle3D[2].y;
  Arete23[2].x := QTriangle3D[3].x; Arete23[2].y := QTriangle3D[3].y;

  Arete31[1].x := QTriangle3D[3].x; Arete31[1].y := QTriangle3D[3].y;
  Arete31[2].x := QTriangle3D[1].x; Arete31[2].y := QTriangle3D[1].y;


 // if Intersect(Transect,Arete12,Ix, Iy) then



end;

// intersection entre un plan horizontal et un segment
function IntersectPlanHorizontalSegment(const QZ: double; const Segmt: TGeoSegment3D; out iX, iY: double): boolean;
var

  dx, dy, dz: double;
  r: double;
begin
  Result := False;
  if (Min(Segmt[1].Z, Segmt[2].Z) > QZ) or (Max(Segmt[1].Z, Segmt[2].Z) < QZ) then exit;
  dx := Segmt[2].x - Segmt[1].x;
  dy := Segmt[2].y - Segmt[1].y;
  // segment vertical ? OK, on sort                       OK
  if (Abs(dx) < Epsilon_High) and
     (Abs(dy) < Epsilon_High) then begin
    ix := Segmt[1].x;
    iy := Segmt[1].y;
    Result := True;
    Exit;
  end;
  dz := Segmt[2].z - Segmt[1].z;
  // segment horizontal -> résultat insensé. On sort      OK
  if (Abs(dz) < Epsilon_High) then Exit;
  // cas général                                          OK
  r  := (QZ - Segmt[1].z) / dz;

  iX := Segmt[1].X + r * dx;
  iY := Segmt[1].Y + r * dy;
  Result := True;



end;

// calcul de l'intersection entre un plan horizontal de cote Z
// indispensable pour les courbes de niveau
// valeur de retour: Le plan passe par le triangle ?
// Ceci est OK.
function IntersectPlanHorizontalTriangle(const QZ: double;
                                         const QTriangle3D: TGeoTriangle3D;
                                         out IntersectP1, IntersectP2: TGeoPoint3D): boolean;
var
  ICnt: integer;
  S1, S2, S3   : TGeoSegment3D;
  Ix, Iy: double;
  HZ, HN: double;
begin
  Result := False;
  
  HZ := -1E12; HN := 1E12;
  for ICnt := 1 to 3 do begin
    if (QTriangle3D[ICnt].z > HZ) then HZ := QTriangle3D[ICnt].Z;
    if (QTriangle3D[ICnt].z < HN) then HN := QTriangle3D[ICnt].Z;

  end;
  if Not ((QZ < HZ) and (QZ > HN)) then Exit;
  //*)
  // cotes des points d'intersection = z
  IntersectP1.z := QZ; IntersectP2.z := QZ;
  // arête de la proj hz du triangle
  S1[1] := QTriangle3D[1];
  S1[2] := QTriangle3D[2];

  S2[1] := QTriangle3D[2];
  S2[2] := QTriangle3D[3];

  S3[1] := QTriangle3D[1];
  S3[2] := QTriangle3D[3];

  


  // intersection avec arête 1 ?
  if IntersectPlanHorizontalSegment(QZ, S1, Ix, Iy) then begin
    IntersectP1.X := Ix; IntersectP1.Y := Iy;

    if IntersectPlanHorizontalSegment(QZ, S2, Ix, Iy) then begin
      IntersectP2.X := Ix; IntersectP2.Y := Iy;
      Result := True;
      //Exit;
    end else begin
      IntersectPlanHorizontalSegment(QZ, S3, Ix, Iy);
      IntersectP2.X := Ix; IntersectP2.Y := Iy;
      Result := True;
      //Exit;
    end;
    Exit;
  end
  // intersection avec arête 2 ?
  else
  if IntersectPlanHorizontalSegment(QZ, S2, Ix, Iy) then begin
    IntersectP1.X := Ix; IntersectP1.Y := Iy;
    if IntersectPlanHorizontalSegment(QZ, S3, Ix, Iy) then begin
      IntersectP2.X := Ix; IntersectP2.Y := Iy;
      Result := True;
      //Exit;
    end else begin
      IntersectPlanHorizontalSegment(QZ, S1, Ix, Iy);
      IntersectP2.X := Ix; IntersectP2.Y := Iy;
      Result := True;
      //Exit;
    end;
    Exit;
  end
  else
  // intersection avec arête 3 ?
  if IntersectPlanHorizontalSegment(QZ, S3, Ix, Iy) then begin
    IntersectP1.X := Ix; IntersectP1.Y := Iy;
    if IntersectPlanHorizontalSegment(QZ, S1, Ix, Iy) then begin
      IntersectP2.X := Ix; IntersectP2.Y := Iy;
      Result := True;
      //Exit;
    end else begin
      IntersectPlanHorizontalSegment(QZ, S2, Ix, Iy);
      IntersectP2.X := Ix; IntersectP2.Y := Iy;
      Result := True;
      //Exit;
    end;
    Exit;
  end;
end;




initialization

  SystemEpsilon :=  CalculateSystemEpsilon;
  MaximumX      :=  Infinity;
  MinimumX      := -Infinity;
  MaximumY      :=  Infinity;
  MinimumY      := -Infinity;
  MaximumZ      :=  Infinity;
  MinimumZ      := -Infinity;

  InitialiseTrigonometryTables;


  (* System precision sanity checks *)
  (*
     Assert(LessThanOrEqual(SystemEpsilon,Epsilon),#13+'WARNING - System epsilon is greater than static epsilon'+ #13 +
                                                       'Accuracy and robustness of calculations that use epsilon for equivalency operations may vary - hence cannot be guaranteed.'+#13);

     Assert(ZeroEquivalency,#13+'WARNING - Pseudo zero equivalency test failed!');

     Assert(ExtendedFloatingPointTest,#13+'WARNING - Extended floating point test has failed!');
  *)

finalization

  finalize(CosTable);
  finalize(SinTable);
  finalize(TanTable);

end.
