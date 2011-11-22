package org

import zhang.geom.Vec3
import toxi.geom.Vec3D

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 11/20/11
 * Time: 6:39 PM
 */

package object zhang {

  implicit def zhang2toxi(z: Vec3) = new Vec3D(z.x, z.y, z.z)
  implicit def toxi2zhang(t: Vec3D) = Vec3(t.x, t.y, t.z)


}