package org.zhang

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 11/20/11
 * Time: 4:10 PM
 */

import geom.{Vec2, Vec3}
import lib.{CP5Util, P5Util, MyPApplet}
import processing.core._
import peasy.PeasyCam
import remixlab.proscene.Scene
import collection.SeqProxy
import toxi.geom.{Spline3D, Vec3D}
import scala.collection.JavaConversions._
import java.text.DecimalFormat
import controlP5.{Controller, Textfield, ControllerInterface, ControlP5, ControlListener, ControlEvent}

class Applet extends MyPApplet { app =>
  import PApplet._; import PConstants._;
  implicit def zhang2toxi(z:Vec3) = new Vec3D(z.x, z.y, z.z)
  implicit def toxi2zhang(t:Vec3D) = Vec3(t.x, t.y, t.z)
  implicit def d2f(d:Double) = d.toFloat

  //==========FIELDS THAT DON'T DEPEND ON UI STATE============

  case class Material(name:String, color:Int, centsPerLinearInch:Int)
  case object BirchVeneer extends Material("Birch Veneer", 0xFFEAA400, 35)
  case object CherryVeneer extends Material("Cherry Veneer", 0xFFFF9C42, 35)
  case object RicePaper extends Material("Rice Paper", 0xFFF8E9FC, 15)
  case object WhitePlastic extends Material("White Plastic", 0xFFFFFFFF, 15)
  case object PinkPlastic extends Material("Pink Plastic", 0xFFFE67EB, 15)
  case object OrangePlastic extends Material("Orange Plastic", 0xFFFF6600, 15)
  val ALL_MATERIALS = List(BirchVeneer, CherryVeneer, RicePaper, WhitePlastic, PinkPlastic, OrangePlastic)//.map(x => (x.name, x)).toMap

  case class Hardware(name:String, cents:Int)
  case object CordWhite6 extends Hardware("6' Cord - White", 10*100)
  case object CordWhite12 extends Hardware("12' Cord - White", 10*100)
  case object Stand24 extends Hardware("24' Stand", 25 * 100)
  case object Stand60 extends Hardware("60' Stand", 25 * 100)
  case object NoHardware extends Hardware("None - I'll get my own", 0)
  val ALL_HARDWARE = List(CordWhite6, CordWhite12, Stand24, Stand60, NoHardware)

  /*
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
  case object ClearSnaps extends Snaps("Clear", color(128, 128))
  case object WhiteSnaps extends Snaps("White", color(255))
  case object BlackSnaps extends Snaps("Black", color(0))
  case object Grommets extends Fastener("Grommets") {

    private val numSteps = 24;
    private val points:Seq[Vec2] = for(theta <- Range.Double(0, TWO_PI, TWO_PI/numSteps)) yield Vec2.fromPolar(1, theta.toFloat)

    def draw(g: PGraphics3D) {
      g.beginShape(TRIANGLE_STRIP);
      for(v <- points) {
        g.vertex(2*v.x, 2*v.y)
        g.vertex(v.x, v.y);
      }
      g.vertex(2*points.head.x, 2*points.head.y)
      g.vertex(points.head.x, points.head.y)
      g.endShape();
    }
  }
  val ALL_FASTENERS = List(ClearSnaps, WhiteSnaps, BlackSnaps, Grommets)
  */


  val spline1 = new Spline3D(Array(
    new Vec3D(.5, 0, 0),
    new Vec3D(1, .5, 0),
    new Vec3D(.5, 1, 0),
    new Vec3D(0, .5, 0),
    new Vec3D(.5, 0, 0)
//      new Vec3D(),
//      new Vec3D(.1f, .8f, .3f),
//      new Vec3D(1, 1, .5f),
//      new Vec3D(.8f, .1f, .3f),
//      new Vec3D()
  ))
  private def sample(s:Spline3D) = {
    val arr = s.computeVertices(5).toArray(Array[Vec3D]())
    Array.tabulate(arr.length)(i => arr(i):Vec3)
  }

  /**
   * A set of sampled points of the first spline, in local coordinates.
   */
  private val s1Sampled:Array[Vec3] = sample(spline1)

  val spline2 = new Spline3D(Array(
    new Vec3D(.5, .25, .1),
    new Vec3D(.75, .5, .1),
    new Vec3D(.5, .75, .1),
    new Vec3D(.25, .5, .1),
    new Vec3D(.5, .25, .1)
  ))

  /**
   * A set of sampled points of the second spline, in local coordinates.
   */
  private val s2Sampled:Array[Vec3] = sample(spline2)


  lazy val buffer = createGraphics(width, height, P3D).asInstanceOf[PGraphics3D]
  object scene extends Scene(this, buffer) {
    disableKeyboardHandling();
    setAxisIsDrawn(false);
    setGridIsDrawn(false);
  }
  override def mousePressed() {
    //camera shouldnt respond to UI events
    scene.enableMouseHandling(!CP5Util.inCP5(mouseX, mouseY, cp5))
  }

  /**
   * Calculates the total length of any sequence of points.
   */
  def calculateLength(pts:Seq[Vec3]) = pts.sliding(2).map(x => (x(1)-x(0)).mag).sum
  def mm1[A, R](f: A => R) = {
    var input:A = null.asInstanceOf[A];
    var value:R = null.asInstanceOf[R];
    (a:A) => if(a == input) value else {
      input = a;
      value = f(a);
      value
    }
  }
  def mm2[A, A2, R](f: (A, A2) => R) = {
    var inputA:A = null.asInstanceOf[A];
    var inputA2:A2 = null.asInstanceOf[A2];
    var value:R = null.asInstanceOf[R];
    (a:A, a2:A2) => if(a == inputA && a2 == inputA2) value else {
      inputA = a; inputA2 = a2;
      value = f(a, a2);
      value
    }
  }

  //====================================================FIELDS THAT DEPEND ON STATE==============================

  /**
   * The UI state is the set of variables for the UI. They are: <ul>
   *   <li>SCALE_XY
        <li>SCALE_Z <br />
   <li>NUM_LAT
  <li>NUM_LON <br />
   <li>PROJECTION
  <li>MATERIAL
  <li>HARDWARE
  </ul>
   */
  object cp5 extends ControlP5(this) {
    //==========================INDEPENDENT STUFF===========================
    /**
     * Converts USD money expressed in cents into a string representation of the cost.
     */
    private val toCost = { val formatter = new DecimalFormat("$#####.00"); (cents:Float) => formatter.format(cents / 100) }
    override def register(ci:ControllerInterface) {
      super.register(ci);
      if(ci.isInstanceOf[Controller]) {
        ci.asInstanceOf[Controller].setMoveable(false);
      }
    }

    private object infoUpdater {
      var infos:Map[Textfield, () => String] = Map()
      def update() { infos.foreach{case (tf, method) => tf.setValue(method())} } //hmm...
    }

    def makeButtons(title:String, y:Float, names:String*) = {
      val label = addTextlabel(title+"title", title, 10, y.toInt);
      val rad = addRadioButton(title, 10, y.toInt+label.getHeight);
      rad.setItemWidth(15); rad.setItemHeight(15);
      rad.setSpacingColumn(80)
      rad.setItemsPerRow(2);
      names.zipWithIndex.foreach{ case(x, i) => {
        val toggle = rad.addItem(x, i)
      }}
      rad.activate(0);
      rad
    }
    /**
     * Converts a given lat and lon into a point on the unit sphere.
     * lat ranges [-PI/2, PI/2], where -PI/2 is the negative Z axis.
     * lon ranges [0, TWO_PI).
     */
    def toSphere(lat:Float, lon:Float) = Vec3.fromSpherical(1, lon, lat);

//    def addListener[A](listenValue: => A, method: => Unit, c:Controller*) {
//      var lastValue = listenValue
//      c.addListener(new ControlListener {
//        def controlEvent(theEvent: ControlEvent) {
//          val newValue = listenValue
//          if(newValue != lastValue) {
//            lastValue = newValue;
//            method
//          }
//        }
//      })
//    }
    def addListener(method: => Unit, cs:Controller*) {
      for(c <- cs) {
        var lastValue = c.value()
        method
        c.addListener(new ControlListener {
          def controlEvent(theEvent: ControlEvent) {
            val newValue = c.value()
            if(newValue != lastValue) {
              lastValue = newValue;
              method
            }
          }
        })
      }
    }
    //==========================DEPENDENT FORM==============================

    val xyScale = addSlider("XY Scale", 1, 100); xyScale.setValue(50); xyScale.linebreak();
    def SCALE_XY = xyScale.getValue

    val zScale = addSlider("Z Scale", 1, 100); zScale.setValue(35); zScale.linebreak();
    def SCALE_Z = zScale.getValue

    val vertDiv = addSlider("Vertical Divisions", 1, 50); vertDiv.setValue(5); vertDiv.linebreak(); vertDiv.setNumberOfTickMarks(50 - 1 + 1); vertDiv.showTickMarks(false);
    def NUM_LAT = vertDiv.getValue.toInt

    val horizDiv = addSlider("Horizontal Divisions", 2, 50); horizDiv.setValue(5); horizDiv.linebreak(); horizDiv.setNumberOfTickMarks(50 - 2+ 1); horizDiv.showTickMarks(false);
    def NUM_LON = horizDiv.getValue.toInt

    /**
     * Converts a given lat and lon given in index-angles into its corresponding point on the unit sphere.
     * @param lat ranges from [0, NUM_LAT), where 0 is at the "bottom" of the lamp and NUM_LAT is the top.
     * @param lon ranges from [0, NUM_LON), where 0 is the +X axis and we are in a right-handed coordinate system.
     */
    def toSphere(lat:Int, lon:Int):Vec3 = toSphere(map(lat, -1, NUM_LAT+1, -PI/2, PI/2), map(lon, 0, NUM_LON, 0, TWO_PI))

    val projection = addSlider("Projection", 1, 10); projection.setValue(1); projection.linebreak();
    def PROJECTION = projection.getValue

    val materials = makeButtons("Materials", height * .35f, ALL_MATERIALS.map(_.name):_*)
    def MATERIAL = ALL_MATERIALS(materials.value.toInt)

//    val fasteners = makeButtons("Fasteners", height * .6f, ALL_FASTENERS.map(_.name):_*)

    val hardware = makeButtons("Hardware", height * .8f, ALL_HARDWARE.map(_.name):_*)
    def HARDWARE = ALL_HARDWARE(hardware.value.toInt)


    def calculateModules = for(lat <- 0 until NUM_LAT; lon <- 0 until NUM_LON) yield Module(
      toSphere(lat, lon),
      toSphere(lat, lon+1),
      toSphere(lat+1, lon+1),
      toSphere(lat+1, lon));

    def validifyState() {
      //this ensures that at least one "materials" option is always checked.
      if(materials.value() < 0) materials.activate(0);
      if(hardware.value() < 0) hardware.activate(0);

      infoUpdater.update();
    }

    def makeInfos(title:String, y:Float, pairs:(String, () => String)*) = {
              val label = addTextlabel(title+"title", title, width - 170, y.toInt);
              val p = pairs.zipWithIndex.map{ case ((name, method), idx) => {
                val myY = y.toInt + label.getHeight + 20 * idx
                val myLabel = addTextlabel(name+"label", name, width - 160, myY)
                val field = addTextfield("", width - 110, myY, 100, 10);
                field.setUserInteraction(false);
                //calling it at the beginning will cause a self-dependency sometimes
        //        field.setValue(method());
                infoUpdater.infos += field -> method
                (field, label)
              }}
              (label, p)
            }

    def calculateHeight = (toSphere(NUM_LAT, 0).z - toSphere(0, 0).z) * SCALE_Z
    def calculateWidth = 2 * SCALE_XY
    val specs = makeInfos("Specs", height * .2f,
      ("Height", () => calculateHeight+" Inches"),
      ("Width", () => calculateWidth+" Inches"))


    //all costs are in cents.
    /**
     * Returns the cost of the hardware, in cents.
     */
    def hardwareCost = HARDWARE.cents
    def materialCost = 40.4542f * 100
    def cuttingCost = {
      /**
       * The total cutting length of the whole lamp, in inches
       */
      val totalInches = modules.map(_.splineLength).sum //todo: OPTIMIZE
//      val totalInches = 10

      totalInches * MATERIAL.centsPerLinearInch
    }
    /**
     * Returns the total cost of the lamp, in cents.
     */
    def totalCost = materialCost + cuttingCost + hardwareCost /*+ fastenersCost*/

    val costs = makeInfos("Fabrication Costs", height * .4f,
      ("Material", () => toCost(materialCost)),
      ("Cutting", () => toCost(cuttingCost)),
      ("Hardware", () => toCost(hardwareCost)),
      //      ("Fasteners", () => toCost(fastenersCost)),
      ("TOTAL     ", () => toCost(totalCost))
    )

    val makeItButton = {
      val button = addButton("MAKE IT!", 0, width-170, (height * .7f).toInt, 160, 100)
      button;
    }
  }
  import cp5.{NUM_LAT, NUM_LON, SCALE_XY, SCALE_Z, PROJECTION, MATERIAL, toSphere/*FASTENER, */}

  /**
   * Depends on: NUM_LAT, NUM_LON
   */
  var modules:IndexedSeq[Module] = _

//  def tabs = for(lat <- 0 to NUM_LAT; lon <- 0 until NUM_LON) yield Tab(toSphere(lat, lon)) //we want to go all the way TO NUM_LAT to encompass the very top of the top strip

  /** Consider the "Real world" length, measured in units of inches. We convert between global coordinates and real world coordinates
   *  by simply scaling the coordinate's x/y by SCALE_XY and the coordiante's z by SCALE_Z.
   *  Depends on: SCALE_XY, SCALE_Z
   */
  var glob2RealWorldMat:PMatrix3D = _

  private def glob2RealWorld(g:Vec3) = P5Util.transformed(g, glob2RealWorldMat)
  private def glob2RealWorld(g:Seq[Vec3]):Seq[Vec3] = g.map(glob2RealWorld _)

  override def setup() {
    size(800, 600, JAVA2D)
//    cam;
    buffer; //force
    scene; //force

    cp5; //force


    cp5.addListener(glob2RealWorldMat = { //todo: optimize?
      val m = new PMatrix3D();
      m.scale(SCALE_XY, SCALE_XY, SCALE_Z);
      m
    }, cp5.xyScale, cp5.zScale);
//    buffer.beginDraw();
//    buffer.smooth();
//    buffer.endDraw();
//    cp5.setAutoDraw(false);
    cp5.addListener(modules = cp5.calculateModules, cp5.vertDiv, cp5.horizDiv)
    cp5.addListener(modules.foreach(_.update()), cp5.zScale)
  }

//  def updateState() {
//    modules = cp5.calculateModules
//  }

  override def draw() {
//    println("Frame "+frameCount+"-----------------")
//    updateState();
    cp5.validifyState() //optimize this method

    background(0);

    //begin 3d drawing
    def drawModules() {
      buffer.beginDraw()
      scene.beginDraw()

      buffer.background(0);
      buffer.lights();

//      println("material: "+MATERIAL)
      buffer.noStroke();
      buffer.fill(MATERIAL.color);
      buffer.pushMatrix();
      buffer.applyMatrix(glob2RealWorldMat)
      modules foreach (_.draw(buffer))
      buffer.popMatrix();


//      tabs foreach (_.draw(buffer))

      scene.endDraw();
      buffer.endDraw()

    }
    drawModules(); //optimize this method

//    println(NUM_LAT*NUM_LON+": "+modules.length+", "+modules.filter(_.midpoint.x.isNaN).length)

    image(buffer, 0, 0);

    def drawGui() {
      scene.beginScreenDrawing();
      cp5.draw();
      scene.endScreenDrawing();
    }
//    drawGui();
    println("End frame: "+frameRate)
  }

  case class Module(p1:Vec3, p2:Vec3, p3:Vec3, p4:Vec3) extends SeqProxy[Vec3] {
    //=========FIELDS THAT DON'T DEPEND ON UI STATE==============
    val self = Seq(p1, p2, p3, p4)

    val midpoint = (this reduceLeft (_ + _)) / 4
    val norm = midpoint.normalize

    private val m2gBeforeZScale = {
      val m = new PMatrix3D();
      m.translate(p3.x, p3.y, p3.z);
      m.apply(P5Util.rotatePlaneAtoBMat(Vec3.X, Vec3.Z, (p4 - p3), ((p4 - p3) cross (p2 - p3))))
      m.scale((p4 - p3).mag)
      if(!isGoodMatrix(m)) m.scale(1, 1, -1);
      m
    }

    /**
     * Returns true if the given matrix correctly points outwards.
     */
    private def isGoodMatrix(mat:PMatrix3D) = P5Util.transformed(Vec3(.5f, .5f, 1), mat).mag > 1

    /**
     * Keeps track of updates.
     */
    private var updates:Seq[() => Unit] = Seq()
    private def addUpdate(x: => Unit) { updates :+= (() => x) }

    //===========FIELDS THAT DEPEND ON UI STATE===================
    /**
     * This matrix converts a middle point into a global point.
     */
    private var m2g:PMatrix3D = _
    addUpdate(m2g = {
            val m = m2gBeforeZScale.get;
            m.scale(1, 1, PROJECTION);
            m;
          })

    /**
     * Converts a global point into a middle point.
     */
    private var g2m:PMatrix3D = _
    addUpdate(g2m = {
            val m = new PMatrix3D(m2g)
            val b = m.invert();
            assert(b, "INVERT DIDN'T WORK: "+this+", midpoint: "+midpoint)
            m
          })

    private def global2Middle(v:Vec3) = P5Util.transformed(v, g2m)

    /**
     * This is p2 represented in the middle coordinate system. It fully describes the trapezoid in the middle.
     */
    private var p2m:Vec3 = _
    addUpdate(p2m = global2Middle(p2))

    /*
     * There is a "middle" coordinate system where p3 = (0, 0), p4 = (1, 0), p2 = (x2, y2), and p1 = (x1, y1). The four corners of the module form
     * a quad in the middle coordinate system.
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
    def loc2Global(loc:Vec3) = P5Util.transformed(loc2Middle(loc), m2g);
    private def mutateToGlobal(dest:Array[Vec3], spline:Array[Vec3]) {
//      locs.map(loc2Global _)
      var i = 0; while(i < spline.length) { dest(i) = loc2Global(spline(i)); i += 1; }
    }

    var s1Glob:Array[Vec3] = Array.ofDim(s1Sampled.length)
    var s2Glob:Array[Vec3] = Array.ofDim(s2Sampled.length)
    addUpdate(mutateToGlobal(s1Glob, s1Sampled))
    addUpdate(mutateToGlobal(s2Glob, s2Sampled))
//    addUpdate(s1Glob = loc2Global(s1Sampled))
//    addUpdate(s2Glob = loc2Global(s2Sampled))

    /**
     * Returns the length of the two splines making up this Module, in real world inches.
     */
    var splineLength:Float = _
    addUpdate(splineLength = {
      //Take spline1,
      //  convert it into real world coordinates,
      //    spline1's points are specified in local coordinates; first convert to global and then convert to real world
      //  calculate its length
      val s1RealWorld = glob2RealWorld(s1Glob)
      val s2RealWorld = glob2RealWorld(s2Glob)

      calculateLength(s1RealWorld) + calculateLength(s2RealWorld)
    })

    /**
     * Should be called whenever the UI state changes. This class's state depends on: <li>
     * <ul>PROJECTION
     * </li>
     */
    def update() {
      updates.foreach(_())
    }
    update(); //call to set variable state at the beginning

//    if(!this.map(x => P5Util.transformed(x, g2m)).forall(x => abs(x.z) < .1f))
//      println("Non-planarity: "+this.map(x => P5Util.transformed(x, g2m)))
//
//    if(!this.map(global2Middle _).zip(List(Vec2(1, 1), Vec2.Y, Vec2(), Vec2.X).map(x => loc2Middle(x.xy))).forall{ case (p, conv) => (p-conv).mag < .001f}) {
//      println("G2M != L2M: \n" +
//        "\tGlobal->Middle: "+this.map(global2Middle _)+"\n" +
//        "\tLocal->Middle: "+List(Vec2(1, 1), Vec2.Y, Vec2(), Vec2.X).map(x => loc2Middle(x.xy))+"\n"+
//        "\tp2m="+p2m)
//    }
//
//    if(loc2Middle(Vec3(.5f, .5f, 1)).z < 0) println("I got a "+loc2Middle(Vec3(.5f, .5f, 1)).z+"!");
//    if(loc2Global(Vec3(.5f, .5f, 1f)).mag < 1) println(this+" transforms (.5, .5, 1) into "+loc2Global(Vec3(.5f, .5f, 1f)))


    override def toString = "Module("+p1+", "+p2+", "+p3+", "+p4+")"
    def draw(g:PGraphics3D) {
      import g._
      def v3(p:Vec3) = g.vertex(p.x, p.y, p.z)

//      def drawSplines() {
//        g.beginShape(TRIANGLE_FAN); s1Sampled.map(loc2Global(_)).foreach(v3 _); g.endShape();
//        g.beginShape(TRIANGLE_FAN); s2Sampled.map(loc2Global(_)).foreach(v3 _); g.endShape();
//      }

      /**
       * At every point in the outer spline is paired with a point on the inner spline. There is also a "neighbor" point
       * for every point. Use the grommel algorithm to draw the surface.
       */
      g.beginShape(TRIANGLE_STRIP);
      var i = 0;
      while(i < s1Glob.length) {
        v3(s1Glob(i));
        v3(s2Glob(i));
        i += 1;
      }
      g.endShape();

      def drawFace() {
        g.stroke(0); g.fill(255);
        g.beginShape(QUADS);
        g.fill(255, 0, 0)
        v3(p1);

        g.fill(0, 255, 0)
        v3(p2);

        g.fill(0, 0, 255)
        v3(p3);

        g.fill(255, 255, 0)
        v3(p4);
        g.endShape(CLOSE);
      }
      def drawMouseSphere() {
        def sphere(loc:Vec3) {
          g.pushMatrix();
          g.translate(loc.x, loc.y, loc.z);
          g.sphereDetail(12);
          g.sphere(.1f);
          g.popMatrix();
        }
        g.noStroke();
        g.fill(255, 0, 0);
        val sLoc = loc2Global(Vec3(mouseX * 1f / g.width, mouseY * 1f / g.height, 0))
        if(keyPressed && key == 'z')
          sphere(sLoc)
      }
      def drawNorm() {
      g.stroke(0); g.strokeWeight(4)
      g.line(midpoint.x, midpoint.y, midpoint.z, midpoint.x + norm.x, midpoint.y+norm.y, midpoint.z + norm.z);
      }
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

//      FASTENER.draw(g);
      g.fill(255);
      g.stroke(255);
      val w = 2
      g.box(w, w, w)

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
