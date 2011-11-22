package org.zhang

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 11/20/11
 * Time: 4:10 PM
 */

import geom.{Vec2, Vec3}
import lib.{P5Util, MyPApplet}
import processing.core._
import peasy.PeasyCam
import remixlab.proscene.Scene
import collection.SeqProxy
import toxi.geom.{Spline3D, Vec3D}
import scala.collection.JavaConversions._
import controlP5.{Controller, ControllerInterface, Textfield, ControlP5}

class Applet extends MyPApplet { app =>
  import PApplet._; import PConstants._;

  import cp5.{NUM_LAT, NUM_LON, SCALE_XY, SCALE_Z, TWIST, MATERIAL, FASTENER, toSphere}

  case class Material(name:String, color:Int)
  case object BirchVeneer extends Material("Birch Veneer", 0xFFEAA400)
  case object CherryVeneer extends Material("Cherry Veneer", 0xFFFF9C42)
  case object RicePaper extends Material("Rice Paper", 0xFFF8E9FC)
  case object WhitePlastic extends Material("White Plastic", 0xFFFFFFFF)
  case object PinkPlastic extends Material("Pink Plastic", 0xFFFE67EB)
  case object OrangePlastic extends Material("Orange Plastic", 0xFFFF6600)
  val ALL_MATERIALS = List(BirchVeneer, CherryVeneer, RicePaper, WhitePlastic, PinkPlastic, OrangePlastic)//.map(x => (x.name, x)).toMap

  abstract class Fastener(val name:String) {
    def draw(g:PGraphics3D)
  }
  class Snaps(name:String, color:Int) extends Fastener("Snaps - "+name) {
    def draw(g:PGraphics3D) {
      g.fill(color);
      g.stroke(color);
      val w = 2
      g.box(w, w, w)
    }
  }
  case object ClearSnaps extends Snaps("Clear", color(128, 40))
  case object WhiteSnaps extends Snaps("White", color(255))
  case object BlackSnaps extends Snaps("Black", color(0))
  case object Grommets extends Fastener("Grommets") {

    def draw(g: PGraphics3D) {
      val DETAIL = .1
      // Create a ring by drawing an outer cicle clockwise and an inner circle anticlockwise.
      def ring(cx1:Float, cy1:Float, r1:Float, cx2:Float, cy2:Float, r2:Float)
      {
        g.beginShape();
         buildCircle(cx1,cy1,r1,true);
         buildCircle(cx2,cy2,r2,false);
        g.endShape();
      }

      // Creates a circle using spline curves. Can be drawn either clockwise
      // which creates a solid circle, or anticlockwise that creates a hole.
      def buildCircle(cx:Float, cy:Float, r:Float, isClockwise:Boolean)
      {
        val numSteps = 10;
        val inc = TWO_PI/numSteps;

        if (isClockwise)
        {
          // First control point should be penultimate point on circle.
          g.curveVertex(cx+r*cos(-inc),cy+r*sin(-inc));

          for(theta <- Range.Double(0, TWO_PI-DETAIL, inc).map(_.toFloat))
          {
            g.curveVertex(cx+r*cos(theta),cy+r*sin(theta));
          }
          g.curveVertex(cx+r,cy);

          // Last control point should be second point on circle.
          g.curveVertex(cx+r*cos(inc),cy+r*sin(inc));

          // Move to start position to keep curves in circle.
          g.vertex(cx+r,cy);
        }
        else
        {
          // Move to start position to keep curves in circle.
          g.vertex(cx+r,cy);

          // First control point should be penultimate point on circle.
          g.curveVertex(cx+r*cos(inc),cy+r*sin(inc));

          for(theta <- Range.Double(TWO_PI, DETAIL, -inc).map(_.toFloat)) {
            g.curveVertex(cx+r*cos(theta),cy+r*sin(theta));
          }
          g.curveVertex(cx+r,cy);

          // Last control point should be second point on circle.
          g.curveVertex(cx+r*cos(TWO_PI-inc),cy+r*sin(TWO_PI -inc));
        }
      }

      fill(255); stroke(255);
      ring(0, 0, 2, 0, 0, 1);
    }
  }
  val ALL_FASTENERS = List(ClearSnaps, WhiteSnaps, BlackSnaps, Grommets)

  object cp5 extends ControlP5(this) {

    override def register(ci:ControllerInterface) {
      super.register(ci);
      if(ci.isInstanceOf[Controller]) {
        ci.asInstanceOf[Controller].setMoveable(false);
      }
    }

    val xyScale = addSlider("XY Scale", 1, 100); xyScale.setValue(50); xyScale.linebreak();
    val zScale = addSlider("Z Scale", 1, 100); zScale.setValue(35); zScale.linebreak();
    val vertDiv = addSlider("Vertical Divisions", 1, 50); vertDiv.setValue(5); vertDiv.linebreak(); vertDiv.setNumberOfTickMarks(50 - 1 + 1); vertDiv.showTickMarks(false);
    val horizDiv = addSlider("Horizontal Divisions", 2, 50); horizDiv.setValue(5); horizDiv.linebreak(); horizDiv.setNumberOfTickMarks(50 - 2+ 1); horizDiv.showTickMarks(false);
    val twist = addSlider("Twist", 1, 100); twist.setValue(16); twist.linebreak();
//    val protrude = addSlider("Protrusion", 1, 10); protrude.setValue(1); protrude.linebreak();

    def makeButtons(title:String, y:Float, names:String*) = {
      val label = addTextlabel(title+"title", title, 10, y.toInt);
      val rad = addRadioButton(title, 10, y.toInt+label.getHeight);
      rad.setSpacingColumn(80)
      rad.setItemsPerRow(2);
      names.zipWithIndex.foreach{ case(x, i) => {
        val toggle = rad.addItem(x, i)
      }}
      rad.activate(0);
      rad
    }

    val materials = makeButtons("Materials", height * .35f, ALL_MATERIALS.map(_.name):_*)

    val fasteners = makeButtons("Fasteners", height * .6f, ALL_FASTENERS.map(_.name):_*)

    val hardware = makeButtons("Hardware", height * .8f, "6' Cord - White", "12' Cord - White", "24' Stand", "60' Stand", "None - I'll get my own")

    def updateInfo() {
      infoUpdater.update();
    }
    private object infoUpdater {
      var infos:Map[Textfield, () => String] = Map()
      def update() {
        infos.foreach{case (tf, method) => tf.setValue(method())}
      }
    }

    def makeInfos(title:String, y:Float, pairs:(String, () => String)*) = {
      val label = addTextlabel(title+"title", title, width - 170, y.toInt);
      pairs.zipWithIndex.foreach{ case ((name, method), idx) => {
        val myY = y.toInt + label.getHeight + 20 * idx
        val myLabel = addTextlabel(name+"label", name, width - 160, myY)
        val field = addTextfield("", width - 110, myY, 100, 10);
        field.setUserInteraction(false);
        field.setValue(method());
        infoUpdater.infos += field -> method
      }}
    }

    val specs = makeInfos("Specs", height * .3f,
      ("Height", () => calculateHeight()+" Inches"),
      ("Width", () => calculateWidth()+" Inches"));

    /**
     * Number of modules in one vertical half-plane.
     */
      def NUM_LAT = vertDiv.getValue.toInt

    /**
     * Number of modules in one horizontal plane.
     */
      def NUM_LON = horizDiv.getValue.toInt
      def SCALE_XY = xyScale.getValue
      def SCALE_Z = zScale.getValue
      def TWIST = twist.getValue
//      def PROTRUDE = protrude.getValue
      def MATERIAL = ALL_MATERIALS(materials.value.toInt)
      def FASTENER = ALL_FASTENERS(fasteners.value.toInt)


      def calculateHeight() = {
        (toSphere(NUM_LAT, 0).z - toSphere(0, 0).z) * SCALE_Z
      }

      def calculateWidth() = {
        2 * SCALE_XY
      }

      /**
       * Converts a given lat and lon into a point on the unit sphere.
       * lat ranges [-PI/2, PI/2], where -PI/2 is the negative Z axis.
       * lon ranges [0, TWO_PI).
       */
      def toSphere(lat:Float, lon:Float) = Vec3.fromSpherical(1, lon, lat);

      /**
       * Converts a given lat and lon given in index-angles into its corresponding point on the unit sphere.
       * @param lat ranges from [0, NUM_LAT), where 0 is at the "bottom" of the lamp and NUM_LAT is the top.
       * @param lon ranges from [0, NUM_LON), where 0 is the +X axis and we are in a right-handed coordinate system.
       */
      def toSphere(lat:Int, lon:Int):Vec3 = toSphere(map(lat, -1, NUM_LAT+1, -PI/2, PI/2), map(lon, 0, NUM_LON, 0, TWO_PI))
//    }
  }

//  def offHoriz = TWO_PI/DIV_HORIZ;
//  def offVert = PI/DIV_VERT;

  def modules = for(lat <- 0 until NUM_LAT; lon <- 0 until NUM_LON) yield Module(
    toSphere(lat, lon),
    toSphere(lat, lon+1),
    toSphere(lat+1, lon+1),
    toSphere(lat+1, lon));

  def tabs = for(lat <- 0 to NUM_LAT; lon <- 0 until NUM_LON) yield Tab(toSphere(lat, lon)) //we want to go all the way TO NUM_LAT to encompass the very top of the top strip

  lazy val buffer = createGraphics(width, height, P3D).asInstanceOf[PGraphics3D]
  lazy val scene = new Scene(this, buffer)

  override def setup() {
    size(800, 600, JAVA2D)
//    cam;
    buffer;
    scene;

    cp5;
//    scene.disableKeyboardHandling();
//    scene.setAxisIsDrawn(false);
//    scene.setGridIsDrawn(false);
//    buffer.beginDraw();
//    buffer.smooth();
//    buffer.endDraw();
//    cp5.setAutoDraw(false);
  }


  def spline1 = new Spline3D(Array(
    new Vec3D(),
    new Vec3D(.1f, .8f, .3f),
    new Vec3D(1, 1, .5f),
    new Vec3D(.8f, .1f, .3f),
    new Vec3D()))

//  def spline2 =
//  def surface =

  override def draw() {
    println("Frame "+frameCount+"-----------------")
    background(0);

    //begin 3d drawing
    drawModules();

//    println(NUM_LAT*NUM_LON+": "+modules.length+", "+modules.filter(_.midpoint.x.isNaN).length)

    image(buffer, 0, 0);

    cp5.updateInfo()
    drawGui();
    println("End frame: "+frameRate)
  }

//  override def keyPressed() {
//    if(key == 'q') {
//      println(modules.mkString("\n"))
//      println("------------------------")
//    }
//  }

  def drawGui() {
    scene.beginScreenDrawing();
    cp5.draw();
    scene.endScreenDrawing();
  }

  def drawModules() {
    buffer.beginDraw()
    scene.beginDraw()

    buffer.background(0);
    buffer.lights();

    println("material: "+MATERIAL)
    buffer.noStroke();
    buffer.fill(MATERIAL.color);
    buffer.pushMatrix();
    buffer.scale(SCALE_XY, SCALE_XY, SCALE_Z)
    modules foreach (_.draw(buffer))
    buffer.popMatrix();


    tabs foreach (_.draw(buffer))

    scene.endDraw();
    buffer.endDraw()

  }



  case class Module(p1:Vec3, p2:Vec3, p3:Vec3, p4:Vec3) extends SeqProxy[Vec3] {
    val self = Seq(p1, p2, p3, p4)

    override def toString = "Module("+p1+", "+p2+", "+p3+", "+p4+")"

//    def norms = combinations(3).map(x => (x(0)-x(1)) cross (x(0) - x(2))).toList;
//    def norm2 = norms.toSet()
//    def norm = (norms reduce (_ + _)) / 4
    def norm = midpoint.normalize
    def midpoint = (this reduceLeft (_ + _)) / 4


    if(keyPressed && key == 'w') println((p4-p3).mag)
    /**
     * This matrix converts a middle point into a global point.
     */
    private val m2g = {
      val m = new PMatrix3D();
      m.translate(p3.x, p3.y, p3.z);
      m.apply(P5Util.rotatePlaneAtoBMat(Vec3.X, Vec3.Z, (p4 - p3), ((p4 - p3) cross (p2 - p3))))
      m.scale((p4 - p3).mag)
      if(!isGoodMatrix(m)) m.scale(1, 1, -1);
//      m.scale(1, -1, -1)
      m
    }

    /**
     * Converts a global point into a middle point.
     */
    private val g2m = {
      val m = new PMatrix3D(m2g)
      val b = m.invert();
      assert(b, "INVERT DIDN'T WORK: "+this+", midpoint: "+midpoint)
      m
    }

    /**
     * This is p2 represented in the middle coordinate system. It fully describes the trapezoid in the middle.
     */
    private val p2m = global2Middle(p2)

    if(!this.map(x => P5Util.transformed(x, g2m)).forall(x => abs(x.z) < .1f))
      println("Non-planarity: "+this.map(x => P5Util.transformed(x, g2m)))

    if(!this.map(global2Middle _).zip(List(Vec2(1, 1), Vec2.Y, Vec2(), Vec2.X).map(x => loc2Middle(x.xy))).forall{ case (p, conv) => (p-conv).mag < .001f}) {
      println("G2M != L2M: \n" +
        "\tGlobal->Middle: "+this.map(global2Middle _)+"\n" +
        "\tLocal->Middle: "+List(Vec2(1, 1), Vec2.Y, Vec2(), Vec2.X).map(x => loc2Middle(x.xy))+"\n"+
        "\tp2m="+p2m)
    }

    if(loc2Middle(Vec3(.5f, .5f, 1)).z < 0) println("I got a "+loc2Middle(Vec3(.5f, .5f, 1)).z+"!");
    if(loc2Global(Vec3(.5f, .5f, 1f)).mag < 1) println(this+" transforms (.5, .5, 1) into "+loc2Global(Vec3(.5f, .5f, 1f)))

    def global2Middle(v:Vec3) = P5Util.transformed(v, g2m)

    /*
     * There is a "middle" coordinate system where p3 = (0, 0), p4 = (1, 0), p2 = (x2, 1), and p1 = (x1, 1). (x2 and x1 are unbound variables)
     */

    private def loc2Middle(loc:Vec3) = {
      if(p2m.x == 0) {
        Vec3(loc.x, loc.y * p2m.y, loc.z)
      }
      else {
        val yC = p2m.y / p2m.x * .5f; //gauranteed not to be NaN because p2m.x is not zero
        def dx_over_dy(xL:Float) = (.5f - xL) / yC//PApplet.map(xL, 0, 1, .5f / yC, -.5f / yC);
        val yM = loc.y * p2m.y;
        val xM = .5f + dx_over_dy(loc.x)*(yM-yC);
        Vec3(xM, yM, loc.z)
      }
    }

//    private def loc2Middle(l:Vec3) = l

    /**
     * Returns true if the given matrix correctly points outwards.
     */
    private def isGoodMatrix(mat:PMatrix3D) = P5Util.transformed(Vec3(.5f, .5f, 1), mat).mag > 1

    /**
     * Interprets the given vector as a point in this module's local coordinate system, and converts it into the global (normal) coordinate system.
     */
    def loc2Global(loc:Vec3) = P5Util.transformed(loc2Middle(loc), m2g);

    def draw(g:PGraphics3D) {
      import g._
      def v3(p:Vec3) = g.vertex(p.x, p.y, p.z)

      //draw spline1
      val pts = spline1.computeVertices(20).map(loc2Global(_))
      if(!(keyPressed && key == 'x')) {
        g.beginShape(TRIANGLE_FAN); pts foreach (v3 _); g.endShape();
      }
//      g.strokeWeight(1);

//      g.textSize(12f / SCALE_XY)
//      def txt(s:String, p:Vec3) = g.text(s, p.x, p.y, p.z);


      //draw simple face
//      g.stroke(0); g.fill(255);
//      g.beginShape(QUADS);
//      g.fill(255, 0, 0)
//      v3(p1);
//
//      g.fill(0, 255, 0)
//      v3(p2);
//
//      g.fill(0, 0, 255)
//      v3(p3);
//
//      g.fill(255, 255, 0)
//      v3(p4);
//      g.endShape(CLOSE);

//      g.fill(255);
//      this.zip(List(1,2,3,4)).foreach{ case (v, s) => txt(s.toString, v * (1 + s * .2f))}

//      def sphere(loc:Vec3) {
//        g.pushMatrix();
//        g.translate(loc.x, loc.y, loc.z);
//        g.sphereDetail(12);
//        g.sphere(.1f);
//        g.popMatrix();
//      }
//      g.noStroke();
//      g.fill(255, 0, 0);
//      val sLoc = loc2Global(Vec3(mouseX * 1f / g.width, mouseY * 1f / g.height, 0))
//      if(keyPressed && key == 'z')
//        sphere(sLoc)

//      if(!(sLoc.mag < 3))
//        println("sLoc at "+sLoc+"!")


      //draw normal
//      g.stroke(0); g.strokeWeight(4)
//      g.line(midpoint.x, midpoint.y, midpoint.z, midpoint.x + norm.x, midpoint.y+norm.y, midpoint.z + norm.z);


      def ptCloud() {
        def pt(vv:Vec3) {
          val v = loc2Global(vv);
          g.point(v.x, v.y, v.z)
  //        g.line(v.x, v.y, v.z, v.x+.00001f, v.y, v.z)
        }

        g.strokeWeight(3); g.stroke(255);
        for(x <- Range.Double(0, 1, .2); y <- Range.Double(0, 1, .2); z <- Range.Double(0, 1, .2)) pt(Vec3(x.toFloat, y.toFloat, z.toFloat))
      }
    }

  }

  case class Tab(p:Vec3) {
    /**
     * Precondition: g has the identity transformation matrix.
     * Postcondition: g still has the identity transformation matrix.
     */
    def draw(g:PGraphics3D) {
      import g._
      g.pushMatrix()
      g.translate(p.x * SCALE_XY, p.y * SCALE_XY, p.z * SCALE_Z)
      g.applyMatrix(P5Util.rotateAtoBMat(Vec3.Z, p))

      /**
       * //todo: PUT CURRENT FASTENER DRAW METHOD HERE
       */
      FASTENER.draw(g);
      g.popMatrix()
//      g.pushMatrix()
//      g.translate(p.x, p.y, p.z);
//      g.applyMatrix(P5Util.rotateAtoBMat(Vec3.Z, p))
//      g.noStroke();
//      g.fill(255)
//  //    ellipse(0, 0, .1f, .1f);
//      val w = .03f
//      g.box(w, w, w * SCALE_XY / SCALE_Z)
////      g.rect(-w, -w, w*2, w*2);
//      g.popMatrix()
    }
  }
}
