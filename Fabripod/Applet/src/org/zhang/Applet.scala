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
import remixlab.proscene.Scene
import collection.SeqProxy
import java.text.DecimalFormat
import controlP5.{Controller, Textfield, ControllerInterface, ControlP5, ControlListener, ControlEvent}
import java.awt.event.{FocusEvent, FocusAdapter}

class Applet extends MyPApplet { app =>
  import PApplet._; import PConstants._;
  implicit def d2f(d:Double) = d.toFloat

  //==========FIELDS THAT DON'T DEPEND ON UI STATE============


  /**
   * Class for all materials. It is here where variables such as material name, color and cost are instantiated.
   */
  case class Material(name:String, color:Int, centsPerLinearInch:Int)
  case object BirchVeneer extends Material("Birch Veneer", 0xFFEAA400, 35) //Birch Veneer costs 35 cents per linear inch.
  case object CherryVeneer extends Material("Cherry Veneer", 0xFFFF9C42, 35) //Cherry Veneer similarly costs 35 cents per linear inch.
  case object RicePaper extends Material("Rice Paper", 0xFFF8E9FC, 15) //Rice paper only costs 15 cents per inch
  case object WhitePlastic extends Material("White Plastic", 0xFFFFFFFF, 15)
  case object PinkPlastic extends Material("Pink Plastic", 0xFFFE67EB, 15)
  case object OrangePlastic extends Material("Orange Plastic", 0xFFFF6600, 15)
  val ALL_MATERIALS = List(BirchVeneer, CherryVeneer, RicePaper, WhitePlastic, PinkPlastic, OrangePlastic)//.map(x => (x.name, x)).toMap

  /**
   * Class for all hardware. Right now there's only a name and a cost (again, in cents)
   */
  case class Hardware(name:String, cents:Int)
  case object CordWhite6 extends Hardware("6' Cord - White", 10*100) //6' cord costs 10 dollars
  case object CordWhite12 extends Hardware("12' Cord - White", 10*100) //12' cord also costs 10 dollars
  case object Stand24 extends Hardware("24' Stand", 25 * 100) //stand costs 25 dollars
  case object Stand60 extends Hardware("60' Stand", 25 * 100)
  case object NoHardware extends Hardware("None - I'll get my own", 0) //free
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
    private val points:Seq[Vec2] = for(theta <- Range.Double(0, TWO_PI, TWO_PI/numSteps)) yield Vec2.fromPolar(.05f, theta.toFloat)

    def draw(g: PGraphics3D) {
//      g.beginShape(TRIANGLE_STRIP);
      g.beginShape();
      points foreach (x => g.vertex(x.x, x.y))
//      for(v <- points) {
//        g.vertex(2*v.x, 2*v.y)
//        g.vertex(v.x, v.y);
//      }
//      g.vertex(2*points.head.x, 2*points.head.y)
//      g.vertex(points.head.x, points.head.y)
//      g.endShape();
      g.endShape(CLOSE);
    }
  }
  case object RectFasteners extends Fastener("Rects") {
    def draw(g: PGraphics3D) {
      g.rect(-.05, -.05, .1, .1)
    }
  }
  val ALL_FASTENERS = List(ClearSnaps, WhiteSnaps, BlackSnaps, Grommets)
*/
  private sealed abstract class Spline {
//    def start:Vec3
//    def end:Vec3
    /**
     * Gives the point on the spline as a function of a position parameter f, where 0 is the start and 1 is the end
     */
    def point(f:Float):Vec3

    /**
     * Estimate the length
     */
    def length:Float

//    private def sampled(num:Int) = Range.Double(0, 1, 1d / num) map (point(_))

    /**
     * Returns a list of sampled points along the line; it should have the start as the zeroeth entry,
     * the end as the last entry, and some number of intermediary points (possibly zero) to help describe the
     * shape in more detail. The total length of the seq will be num.
     */
    def sampled(num:Int):Seq[Vec3] = if(num < 2) sys.error("num is "+num+"!") else (0 to num-1) map (x => point(x * 1f / (num-1)))
  }
  private case class Line(start:Vec3, end:Vec3) extends Spline {
    def point(f:Float) = start + (end - start) * f
    def length = start distTo end

    /**
     * Returns a Seq of length num holding points sampled evenly over this line.
     */
//    def sampled(num:Int) =
//    def sampled = Seq(start, end)
  }
  private case class BezierSpline(start:Vec3, cp1:Vec3, cp2:Vec3, end:Vec3) extends Spline {
    def seq = Seq(start, cp1, cp2, end)
    import collection.JavaConverters._
    def point(f:Float) = Vec3(
//      curvePoint(start.x, cp1.x, cp2.x, end.x, f),
//      curvePoint(start.y, cp1.y, cp2.y, end.y, f),
//      curvePoint(start.z, cp1.z, cp2.z, end.z, f)
      bezierPoint(start.x, cp1.x, cp2.x, end.x, f),
      bezierPoint(start.y, cp1.y, cp2.y, end.y, f),
      bezierPoint(start.z, cp1.z, cp2.z, end.z, f)
    )
//    private val repr = new Spline3D(points.map(x => x:Vec3D).toArray) //mutability!
//    def point(f:Float) = repr.
    def length = calculateLength(sampled(10))
//    def sampled(num:Int) = repr.computeVertices(num).asScala.toSeq.map(x => x:Vec3) //mutable!
//    def sampled:Seq[Vec3] = sampled(SAMPLE_NUM)
  }
  private object BezierSpline {
    def apply(pt:Seq[Vec3]):BezierSpline = apply(pt(0), pt(1), pt(2), pt(3))
  }

  /**
   * An arc on the
   */
  private case class ArcSpline(center:Vec3, rad:Float, angStart:Float, angEnd:Float) extends Spline {
    def angleDistance(xx1:Float, xx2:Float) = {
      val x1 = wrap(xx1, TWO_PI);
      val x2 = wrap(xx2, TWO_PI);

      if(x1 == x2) 0f
      else if(x1 > x2) {
          if(x1 - x2 > PI) (x2 + TWO_PI) - x1;
          else -(x1 - x2);
      }
      else {
          if(x2 - x1 > PI) x2 - (x1 + TWO_PI)
          else x2 - x1;
      }
    }
    def wrap(x:Float, wrap:Float) = {
      val mod = x % wrap;
      if(mod < 0) mod + wrap else mod;
    }
    def turnTowardsAngle(anglee:Float, wantAng:Float, cap:Float) = {
      val angle = wrap(anglee, TWO_PI);
      angle + constrain(angleDistance(angle, wantAng), -cap, cap);
    }
    def length = TWO_PI * rad
    private val startRanged = wrap(angStart, TWO_PI)
    private val endRanged = wrap(angEnd, TWO_PI)

    def point(f: Float) =
      Vec2.fromPolar(rad,
        turnTowardsAngle(startRanged, endRanged, f * angleDistance(startRanged, endRanged))).xy + center
  }

/*
  private case class PiecewiseSpline(splines:Spline*) extends Spline {
    def length = (splines map (_.length)).sum

    def point(f:Float) = if(abs(f - 1d) < 1e-5) splines.last.point(1) else {

      //lengths of 5, 5, 5, 25 => (.125, .125, .125, .625)
      // => (0, .125, .250, .375, 1)
      // => e.g. .05 = splines(0).point(map(f, 0, .125, 0, 1))
      //
      val segNorms = (0f +: (org.zhang.lib.partialSum(splines.map(_.length / length).toStream).toSeq))
      val pairs = segNorms.sliding(2).toSeq
      var goodIndex = pairs.indexWhere(f < _(1))
      if(goodIndex < 0 || goodIndex >= pairs.length || goodIndex >= splines.length) {
        println("aww skeet")
        goodIndex = 0;
      }
      splines(goodIndex).point(map(f, pairs(goodIndex)(0), pairs(goodIndex)(1), 0, 1))
//      val lenSummed = org.zhang.lib.partials(lenNorm.toStream).toSeq.map(stream => (stream.last._1, stream.map(_._2).sum)) //gives the summed up length of all the splines
//      val lenDropped = lenSummed.dropWhile(_._2 < f) //drops all splines that are before the wanted point
//      lenDropped.head._1.point(map(f, ))
    }
  }
*/
  /**
   * The height of the control points for the bezier paths.
   */
  private val segLen = .1f

  lazy val buffer = createGraphics(width, height, P3D).asInstanceOf[PGraphics3D]
  object scene extends Scene(this, buffer) {
    disableKeyboardHandling();
    setAxisIsDrawn(false);
    setGridIsDrawn(false);
    cam.setPosition(new PVector(0, 261, 0))
    cam.lookAt(new PVector());
  }
  override def mousePressed() {
    //camera shouldnt respond to UI events
    scene.enableMouseHandling(!CP5Util.inCP5(mouseX, mouseY, cp5))
  }

  /**
   * Calculates the total length of any sequence of points.
   */
  def calculateLength(pts:Seq[Vec3]) = pts.sliding(2).map(x => (x(1)-x(0)).mag).sum

  //====================================================FIELDS THAT DEPEND ON STATE==============================


  /**
   * The UI state is the set of variables for the UI. They are: <ul>
   *   <li>SCALE_XY
        <li>SCALE_Z
        <li>ARC_HEIGHT
        <li>CIRCLE_RAD
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
    /**
     * The method will get called whenever the controller's <code>.value()</code> changes.
     */
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

    val xyScale = addSlider("XY Scale", 1, 25); xyScale.setValue(5); xyScale.linebreak();
    def SCALE_XY = xyScale.getValue

    val zScale = addSlider("Z Scale", 1, 25); zScale.setValue(5); zScale.linebreak();
    def SCALE_Z = zScale.getValue

    val vertDiv = addSlider("Vertical Divisions", 1, 50); vertDiv.setValue(5); vertDiv.linebreak(); vertDiv.setNumberOfTickMarks(50 - 1 + 1); vertDiv.showTickMarks(false);
    def NUM_LAT = vertDiv.getValue.toInt

    val horizDiv = addSlider("Horizontal Divisions", 2, 50); horizDiv.setValue(5); horizDiv.linebreak(); horizDiv.setNumberOfTickMarks(50 - 2+ 1); horizDiv.showTickMarks(false);
    def NUM_LON = horizDiv.getValue.toInt

    val arcHeight = addSlider("Arc Height", 0, 1); arcHeight.setValue(.2f); arcHeight.linebreak();
    def ARC_HEIGHT = arcHeight.getValue

    val circleRad = addSlider("Circle Radius", 0, .5f); circleRad.setValue(.2f); circleRad.linebreak();
    def CIRCLE_RAD = circleRad.getValue

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

    def validifyState() {
      //this ensures that at least one "materials" option is always checked.
      if(materials.value() < 0) materials.activate(0);
      if(hardware.value() < 0) hardware.activate(0);

      infoUpdater.update();
    }

    def makeInfos(title:String, y:Float, pairs:(String, () => String)*) = {
              val label = addTextlabel(title, title, width - 170, y.toInt);
              val p = pairs.zipWithIndex.map{ case ((name, method), idx) => {
                val myY = y.toInt + label.getHeight + 20 * idx
                val myLabel = addTextlabel(name, name, width - 160, myY)
                val field = addTextfield("", width - 110, myY, 100, 10);
                field.setUserInteraction(false);
                //calling it at the beginning will cause a self-dependency sometimes
        //        field.setValue(method());
                infoUpdater.infos += field -> method
                (field, myLabel)
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
      val totalInches = modules.map(_.splineLength).sum
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
      button.addListener(new ControlListener {
        def controlEvent(theEvent: ControlEvent) {
          sendEmail()
        }
      })
      button;
    }
  }
  import cp5.{NUM_LAT, NUM_LON, SCALE_XY, SCALE_Z, PROJECTION, CIRCLE_RAD, ARC_HEIGHT, MATERIAL, toSphere/*FASTENER, */}

  def sendEmail() {
    def showError(e:Exception) {
      e.printStackTrace()
      val err = e.toString
      val cw = cp5.addControlWindow("err "+err, 800, 200)
      val canvas = new controlP5.ControlWindowCanvas() {
        def draw(p: PApplet) {
          p.background(0);
          p.stroke(255);
          p.textFont(p.createFont("Arial", 20))
          p.text("An error has occured: \n"+err+"\nPlease contact chris@fabripod.com for more details.", 0, 0, p.width, p.height)
        }
      }
      cw.addCanvas(canvas)
    }
    val transactionCode = (new scala.util.Random()).nextLong().toHexString
    def makeEmailText() = {
      //XY_SCALE
      //Z_SCALE
      //vertical divisions
      //horizontal divisions
      //arc height
      //circle radius
      //projection

      //material
      //hardware

      //width
      //height

      //costs


      val dt = "Date and time: "+java.text.DateFormat.getDateTimeInstance.format(new java.util.Date());
      val tCode = "Transaction code: "+transactionCode

      val sliders = {import cp5._; List(xyScale, zScale, vertDiv, horizDiv, arcHeight, circleRad, projection)}.map(x => x.name()+": "+x.value()).mkString("Parameters\n\t", "\n\t", "")

      val material = "Material: "+cp5.MATERIAL.name

      val hardware = "Hardware: "+cp5.HARDWARE.name

      import controlP5.Textlabel
      def makeInfosString(infos:(Textlabel, Seq[(Textfield, Textlabel)])) = infos match {
        case (label, seq) => label.label+"\n"+(seq.map{ case (field, label) => label.label.trim+": "+field.getText}.mkString("\t", "\n\t", ""))
      }

      val specs = makeInfosString(cp5.specs)

      val costs = makeInfosString(cp5.costs)

      List(dt, tCode, sliders, material, hardware, specs, costs).mkString("\n\n")
    }
    println("email text will look like: \n"+makeEmailText())
    import java.io._
    import java.net._
    import javax.mail._, internet._
    import java.util.Properties
    try {
      val props = new Properties();
      props.put("mail.smtp.port", "587");
//      props.put("mail.smtp.host", )
      props.put("mail.smtp.user", "sales@fabripod.com")
      props.put("mail.smtp.starttls.enable", "true");
      props.put("mail.smtp.auth", "true");
      props.put("mail.smtp.recipient", "chris@fabripod.com")
      //val to = "chris@fabripod.com"

      try {
        val propsFile = new URL(getCodeBase(), "email.txt");
        val in = propsFile.openStream();
        props.load(in);
        in.close();
      } catch {
        case e: Exception => showError(e)
      }

      val host = props.getProperty("mail.smtp.host") //what if these are null?
      val from = props.getProperty("mail.smtp.user")
      val pass = props.getProperty("mail.smtp.password") //what if this is null? what if it's malformed?
      val to = props.getProperty("mail.smtp.recipient")

      val session = Session.getDefaultInstance(props, null);
      val message = new MimeMessage(session);
      message.setFrom(new InternetAddress(from));
      val toAddr = new InternetAddress(to)
      message.addRecipient(Message.RecipientType.TO, toAddr)
      message.setSubject("Lamp transaction")
      message.setText(makeEmailText());
      val transport = session.getTransport("smtp")
      transport.connect(host, from, pass)
      transport.sendMessage(message, message.getAllRecipients());
      transport.close();
    } catch {
      case e: Exception => showError(e)
    }
  }

  /**
   * The radius of the upper circle, from the center
   */
  private def upperRad = CIRCLE_RAD

  /**
   * The height of the upper circle.
   */
  private def upperCircleHeight = map(CIRCLE_RAD, 0, cp5.circleRad.getMax, .2f, .35f)


  /**
   * A set of sampled points of the first spline, in local coordinates. depends on: arcHeight, circleRad
   */
  private var s1Sampled:Array[Vec3] = _
  private var s2Sampled:Array[Vec3] = _

  /**
   * Depends on: NUM_LAT, NUM_LON
   */
  var modules:IndexedSeq[Module] = _
  def yieldModules = for(lat <- 0 until NUM_LAT; lon <- 0 until NUM_LON) yield Module(
        toSphere(lat, lon),
        toSphere(lat, lon+1),
        toSphere(lat+1, lon+1),
        toSphere(lat+1, lon));

  /**
   * Depends on: NUM_LAT, NUM_LON
   * tabs goes from [0, NUM_LAT]; each tabs(i) goes from [0, NUM_LON)
   */
  var tabs:IndexedSeq[Tab] = _
    //for(lat <- 0 to NUM_LAT; lon <- 0 until NUM_LON) yield Tab(toSphere(lat, lon)) //we want to go all the way TO NUM_LAT to encompass the very top of the top strip
  def yieldTabs = for(lat <- 0 to NUM_LAT; lon <- 0 until NUM_LON) yield {
    /**
     * @param lat [0, NUM_LAT)
     */
    def moduleAt(lat:Int, lon:Int) =
      if(lat >= 0 && lat < NUM_LAT)
        modules(lat*NUM_LON+(if(lon < 0) lon + NUM_LON else lon % NUM_LON))
      else
        sys.error(lat+" out of bounds")

    val segLen = this.segLen * 1.1f
    val pts = lat match {
      case 0 => {
        Seq(
          moduleAt(lat, lon).loc2Global(Vec2(1 - segLen, 1)), //left
          moduleAt(lat, lon-1).loc2Global(Vec2(segLen, 1)), //right
          moduleAt(lat, lon-1).loc2Global(Vec2(0, 1 - segLen)) //top
        )
      }
      case lat if lat == NUM_LAT => {
        Seq(
          moduleAt(lat-1, lon).loc2Global(Vec2(1 - segLen, 0)), //left
          moduleAt(lat-1, lon-1).loc2Global(Vec2(segLen, 0)), //right
          moduleAt(lat-1, lon-1).loc2Global(Vec2(0, segLen)) //bottom
        )
      }
      case lat => {
        Seq(moduleAt(lat, lon).loc2Global(Vec2(1, 1 - segLen)), //top
          moduleAt(lat - 1, lon).loc2Global(Vec2(1 - segLen, 0)), //left
          moduleAt(lat - 1, lon - 1).loc2Global(Vec2(0, segLen)), //bottom
          moduleAt(lat, lon - 1).loc2Global(Vec2(segLen, 1))) //right
      }
    }
    Tab(pts) //we want to go all the way TO NUM_LAT to encompass the very top of the top strip
  }

  /** Consider the "Real world" length, measured in units of inches. We convert between global coordinates and real world coordinates
   *  by simply scaling the coordinate's x/y by SCALE_XY and the coordiante's z by SCALE_Z.
   *  Depends on: SCALE_XY, SCALE_Z
   */
  var glob2RealWorldMat:PMatrix3D = _

  private def glob2RealWorld(g:Vec3) = P5Util.transformed(g, glob2RealWorldMat)
  private def glob2RealWorld(g:Seq[Vec3]):Seq[Vec3] = g.map(glob2RealWorld _)

  override def setup() {
    size(800, 600, JAVA2D)
//    assert(s1Sampled.length == s2Sampled.length, s1Sampled.length+" vs "+s2Sampled.length) //this also forces it
//    cam;
    buffer; //force
    scene; //force

    cp5; //force

    app.addFocusListener(new FocusAdapter() {
      override def focusGained(p1: FocusEvent) {
        //send alt-up button
        val keyEventAltUp = new java.awt.event.KeyEvent(app, 402, System.currentTimeMillis(), 0, 18, 65535, 2);
        app.keyReleased(keyEventAltUp)
      }
    })

    cp5.addListener({s1Sampled = {
        val bezTall = ARC_HEIGHT * upperCircleHeight

        val xC = .5f - upperRad
        val bezTallIn = bezTall * xC / dist(0, 0, xC, upperCircleHeight)
        val bezTallUp = bezTall * upperCircleHeight / dist(0, 0, xC, upperCircleHeight)
        val bs1 = List(Vec3(segLen, 0, 0), Vec3(segLen, bezTallIn, bezTallUp), Vec3(1 - segLen, bezTallIn, bezTallUp), Vec3(1 - segLen, 0, 0));
        val bs2 = bs1.map(x => P5Util.transformed(x, {val p = new PMatrix3D; p.translate(1, 0); p.rotate(PI/2);   p}))
        val bs3 = bs1.map(x => P5Util.transformed(x, {val p = new PMatrix3D; p.translate(1, 1); p.rotate(PI);     p}))
        val bs4 = bs1.map(x => P5Util.transformed(x, {val p = new PMatrix3D; p.translate(0, 1); p.rotate(3*PI/2); p}))
        val spline1 = Seq(
              Line(Vec3(0, segLen, 0), Vec3(segLen, 0, 0)),
              BezierSpline(bs1),
              Line(Vec3(1 - segLen, 0, 0), Vec3(1, segLen, 0)),
              BezierSpline(bs2),
              Line(Vec3(1, 1 - segLen, 0), Vec3(1 - segLen, 1, 0)),
              BezierSpline(bs3),
              Line(Vec3(segLen, 1, 0), Vec3(0, 1 - segLen, 0)),
              BezierSpline(bs4)
            )
        def cleanSample(s: Spline) = s match {
          case Line(a, b) => Seq(a, b)
          case b: BezierSpline => b.sampled(10)
        }
        spline1.map(cleanSample _).reduceLeft(_.dropRight(1) ++ _).toArray
      }; s2Sampled = {
          val as1 = new ArcSpline(Vec3(.5, .5, upperCircleHeight), upperRad, -PI*3/4, PI*1/4);
          val as2 = new ArcSpline(Vec3(.5, .5, upperCircleHeight), upperRad, PI*1/4, PI*5/4);
          (as1.sampled(s1Sampled.length/2).dropRight(1) ++ as2.sampled((s1Sampled.length+1)/2+1)).toArray
        }}, cp5.arcHeight, cp5.circleRad);


    cp5.addListener({glob2RealWorldMat = {
      val m = new PMatrix3D();
      m.scale(SCALE_XY, SCALE_XY, SCALE_Z);
      m
    }}, cp5.xyScale, cp5.zScale);
    cp5.addListener({
      modules = yieldModules; //modules are consistent
      tabs = yieldTabs; //tabs are now consistent.
    }, cp5.vertDiv, cp5.horizDiv)
    cp5.addListener(modules.foreach(_.updateFromProjection()), cp5.projection)
    cp5.addListener(modules.foreach(_.updateFromXYZScale()), cp5.xyScale, cp5.zScale)
    cp5.addListener(modules.foreach(_.updateFromArcHeightCircleRadius()), cp5.arcHeight, cp5.circleRad)
    //    buffer.beginDraw();
    //    buffer.smooth();
    //    buffer.endDraw();
    //    cp5.setAutoDraw(false);
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
//      buffer.ambientLight(102, 102, 102);
      buffer.lightSpecular(204, 204, 204);
      buffer.specular(255);
      buffer.directionalLight(180, 180, 180, .4, .6, .25)
      buffer.shininess(10)

//      println("material: "+MATERIAL)
      buffer.noStroke();
      buffer.fill(MATERIAL.color);
      buffer.pushMatrix();
      buffer.applyMatrix(glob2RealWorldMat)
      buffer.scale(10); //draw everything bigger
      modules foreach (_.draw(buffer))
      tabs foreach (_.draw(buffer))
      buffer.popMatrix();


//      buffer.pushMatrix();
//      buffer.scale(10);
//      tabs foreach (_.draw(buffer))
//      buffer.popMatrix();



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
//    println("End frame: "+frameRate)
  }

  /**
   * <p>Represents one module of the lamp, specified by the four corners of the module; it's assumed that the corners form
   * a trapezoid on the plane where the points live.</p> <br />
   * <p>
   * Each module has its own local coordinate system where the +Z direction
   * points in the module's normal, the +X direction points from p3 to p4, and the +Y direction points from p3 to p2. The point
   * (1, 0, 0) in local coordinates transforms into the p4 coordinate. The local coordinate's origin transforms into p3. The point (0, 0, 1)
   * transforms into a coordinate projected outward from p3, of length equal to the length from the trapezoid's middle to the global origin. As such,
   * small z-values are preferred. </p><br />
   * <p>If we
   * imagine looking at the trapezoid in the plane that it lives on, then p1 is the bottom right corner, p2 is the bottom left corner,
   * p3 is the top left corner, and p4 is the top right corner.</p><br />
   * <p> The state of the module will be consistent with the UI parameters in two cases: <ol>
   *   <li> The module gets instantiated for the first time.
   *   <li> The module's <code>updateFromProjection()</code> and <code>updateFromScaleXYZ()</code> methods are called.
   * </ol>
   * It is enough to call such methods only when the corresponding UI elements' values change.</p><br />
   * <p>The only variable that depends on the XYZ scale is the spline length.</p><br />
   * <p>The PROJECTION field affects the coordinate transformation from local to global; specifically, the local plane's Z coordinate
   * gets scaled by <code>PROJECTION</code>. This means that points on the local plane with zero Z component are invariant under UI changes.</p>
   */
  case class Module(p1:Vec3, p2:Vec3, p3:Vec3, p4:Vec3) extends SeqProxy[Vec3] {

    //=========FIELDS THAT DON'T DEPEND ON UI STATE (namely, PROJECTION and XY_SCALE and Z_SCALE)==============
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
     * Keeps track of updates that depend on the PROJECTION field.
     */
    private var updatesProjection:Seq[() => Unit] = Seq()
    private def addUpdateProjection(x: => Unit) { updatesProjection :+= (() => x) }

    //===========FIELDS THAT DEPEND ON UI STATE===================
    /**
     * This matrix converts a middle point into a global point.
     */
    private var m2g:PMatrix3D = _
    addUpdateProjection(m2g = {
            val m = m2gBeforeZScale.get;
            m.scale(1, 1, PROJECTION);
            m;
          })

    /**
     * Converts a global point into a middle point.
     */
    private var g2m:PMatrix3D = _
    addUpdateProjection(g2m = {
            val m = new PMatrix3D(m2g)
            val b = m.invert();
            assert(b, "INVERT DIDN'T WORK: "+this+", midpoint: "+midpoint)
            m
          })

    private def global2Middle(v:Vec3) = P5Util.transformed(v, g2m)

    /**
     * This is p2 represented in the middle coordinate system. It fully describes the trapezoid in the middle. This vector
     * should have zero Z component in both middle and local systems.
     */
    private var p2m:Vec3 = _
    addUpdateProjection(p2m = global2Middle(p2))

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
    def loc2Global(loc:Vec2):Vec3 = P5Util.transformed(loc2Middle(loc.xy), m2g);

    /**
     * Depends on: ARC_HEIGHT, CIRCLE_RAD, PROJECTION
     */
    var s1Glob:Array[Vec3] = _//Array.ofDim(s1Sampled.length)
    var s2Glob:Array[Vec3] = _//Array.ofDim(s2Sampled.length)
    private def updateGlobs() {
      /**
       * Maps all the splines from local to global coordinates and mutates the dest array to hold such values.
       */
      def mutateToGlobal(dest:Array[Vec3], spline:Array[Vec3]) {
        var i = 0; while(i < spline.length) {
          dest(i) = loc2Global(spline(i)); i += 1;
        }
      }
      if(s1Glob == null) s1Glob = Array.ofDim(s1Sampled.length)
      if(s2Glob == null) s2Glob = Array.ofDim(s2Sampled.length)
      mutateToGlobal(s1Glob, s1Sampled)
      mutateToGlobal(s2Glob, s2Sampled)
    }
    addUpdateProjection(updateGlobs())
//    addUpdate(s1Glob = loc2Global(s1Sampled))
//    addUpdate(s2Glob = loc2Global(s2Sampled))

    /**
     * Returns the length of the two splines making up this Module, in real world inches.
     * DEPENDS ON SCALE_XY, SCALE_Z, PROJECTION, ARC_HEIGHT, CIRCLE_RAD
     */
    var splineLength:Float = _
    private def updateSplineLength() {
      splineLength = {
          //Take spline1,
          //  convert it into real world coordinates,
          //    spline1's points are specified in local coordinates; first convert to global and then convert to real world
          //  calculate its length
          val s1RealWorld = glob2RealWorld(s1Glob)
          val s2RealWorld = glob2RealWorld(s2Glob)

          calculateLength(s1RealWorld) + calculateLength(s2RealWorld)
        }
    }
    addUpdateProjection(updateSplineLength())

    def updateFromArcHeightCircleRadius() {
      updateGlobs()
      updateSplineLength()
    }

    def updateFromXYZScale() {
      updateSplineLength()
    }

    /**
     * Should be called whenever the UI state changes. This class's state depends on: <li>
     * <ul>PROJECTION
     * </li>
     */
    def updateFromProjection() {
      updatesProjection.foreach(_())
    }
    updateFromProjection(); //call to set variable state at the beginning

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
        if((keyPressed:Boolean) && key == 'z')
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

//  class Tab(var top:Vec3, var left:Vec3, var bottom:Vec3, var right:Vec3) {
  /**
   * <p>Represents one tab of the lamp; as of now, this is simply a wrapper around a list of points that the tab should draw to.</p><br />
   * <p>All values are interpreted in global coordinates (NOT real world).</p><br />
   * <p>This class is invariant under all UI states (bar horizDiv and vertDiv of course).</p>
   */
  case class Tab(pts:Seq[Vec3]) {
    /**
     * Precondition: g has the same transformation as was used for modules, WITHOUT the glob2RealWorld matrix.
     * Postcondition: g still has the identity transformation matrix.
     */
    def draw(g:PGraphics3D) {
      import g._
//      g.pushMatrix()
      g.fill(MATERIAL.color)
      g.beginShape(TRIANGLE_FAN);
//      List(top, left, bottom, right) foreach (x => g.vertex(x.x, x.y, x.z))
      pts foreach (x => g.vertex(x.x, x.y, x.z))
      g.endShape(CLOSE);
//      g.popMatrix()
    }
  }
}
