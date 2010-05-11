
object Identicon {
  import java.awt.geom.AffineTransform
  import swing._


  class RichGraphics {
    def fillBlock(b:Block) = {
      val t = g.getTransform
      val c = g.getColor
      val at = new AffineTransform
      val (fc, bc) = Option(b.color -> g.getBackground)
                        .map(t => if (b.invert) t.swap else t).get
      at.translate(2, 2)
      at.concatenate(b.transform)
      g.transform(at)
      g.setColor(bc)
      g.fill(RichGraphics.rect)
      g.setColor(fc)
      g.fillPolygon(b)
      g.setTransform(t)
      g.setColor(c)
    }
  }
  object RichGraphics {
    val rect = new java.awt.Rectangle(-2, -2, 4, 4)
  }

  /** Add rendering Block function to Graphics2D */
  implicit def enrichg2(g:Graphics2D) = new RichGraphics


  class BitStream(var value:Int) {
    def shift(n:Int) = {
      val v = value & ((1 << n) - 1)
      value >>= n
      v
    }
    def shiftArray(n:Int, count:Int) =
      (1 to count).map(i => shift(n)).toArray
  }

  /** Direction */
  object Dir extends Enumeration {
    type Dir = Dir.Value
    val Up, Left, Down, Right = Value
  }
  import Dir.Dir


  class Block(val transform:AffineTransform, val color:Color,
               val invert:Boolean, points:(Int, Int)*) extends java.awt.Polygon() {
    def this(ps:(Int, Int)*) = this(new AffineTransform(), java.awt.Color.black, false, ps:_*)

    points.foreach(p => addPoint(p._1, p._2))

    def copy(dx:Int, dy:Int, dr:Dir = Dir.Up,
             color:Color = this.color, invert:Boolean = this.invert):Block = {
      val at = new AffineTransform
      at.translate(dx, dy)
      at.concatenate(transform)
      at.rotate(math.Pi / 2.0 * dr.id)
      new Block(at, color, invert, points:_*)
    }
  }

  object Block {
    def apply(points:(Int, Int)*) = new Block(points:_*)

    /** Create center, side, corner Block form 32bit id */
    def decode(code:Int):(Block, Block, Block) = {
      object bit extends BitStream(code)
      val c = new java.awt.Color(
        bit.shift(5) << 3,
        bit.shift(6) << 2,
        bit.shift(5) << 3
      )
      return (
        blocks(bit.shift(2)).copy(0, 0, Dir.Up, c),            // center
        blocks(bit.shift(4)).copy(0, 0, Dir(bit.shift(2)), c, bit.shift(1) == 0),
        blocks(bit.shift(4)).copy(0, 0, Dir(bit.shift(2)), c, bit.shift(1) == 0)
      )
    }
  }

  val blocks = List(
    Block((-2,-2)),
    Block((-2,-2),(2,-2),(2,2),(-2,2)),
    Block((0,-2),(2,0),(0,2),(-2,0)),
    Block((-1,-1),(1,-1),(1,1),(-1,1)),
    Block((-2,-2),(2,-2),(-2,2)),
    Block((0,-2),(2,2),(-2,2)),
    Block((-2,-2),(0,-2),(0,2),(-2,2)),
    Block((-2,-2),(2,0),(2,2),(0,2)),
    Block((-2,-2),(2,-2),(2,0),(0,-2),(-2,0)),
    Block((-2,-2),(2,0),(0,2)),
    Block((0,0),(0,-2),(2,-2),(-2,2),(-2,0)),
    Block((-2,-2),(0,-2),(0,0),(-2,0)),
    Block((-2,0),(2,0),(0,2)),
    Block((0,0),(2,2),(-2,2)),
    Block((0,-2),(0,0),(-2,0)),
    Block((-2,-2),(0,-2),(-2,0))
  )


  object Pattern3x3 {
    def render(g:Graphics2D, code:Int) = {
      def fillAndRotate(b:Block, p:(Int, Int)) = {
        g.fillBlock(b.copy(p._1, p._2))
        b.copy(0, 0, Dir.Left)
      }
      val (center, side, corner) = Block.decode(code)
      (corner /: List((0,0),(8,0),(8,8),(0,8)))(fillAndRotate)
      (side /: List((0,4),(4,0),(8,4),(4,8)))(fillAndRotate)
      (center /: List((4,4)))(fillAndRotate)
    }
  }





  // calc code
//  import java.security.MessageDigest
//
//  val id = "scala".hashCode
//  val md = MessageDigest.getInstance("MD5")
//  val digest = md.digest((new BitStream(id)).shiftArray(8, 4).map(_.toByte))
//  val code = (0 /: digest.take(4)){(s, b) => (s << 8) | (b.toInt & 0xff)}
  val code = 12345678

  // create image
  import java.awt.image.BufferedImage

  val scale = 6
  val size = 4 * 3 * scale
  val img = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
  val g = img.createGraphics
  g.setBackground(java.awt.Color.white)
  g.scale(scale, scale)
  Pattern3x3.render(g, code)

  // show window
  new Frame {
    contents = new BorderPanel {
      preferredSize = new Dimension(300, 300)
      override def paintComponent(g:Graphics2D) =
        g.drawImage(img, 10, 10, null)
    }
    visible = true
  }
}

