package spray.json

import scala.reflect.ClassTag
import scala.collection.mutable

/**
  * Provides snake cased JsonFormats
  */
trait SnakeCaseJsonSupport extends DefaultJsonProtocol {

  override protected def extractFieldNames(classTag: ClassTag[_]): Array[String] =
    super.extractFieldNames(classTag).map(enforceSnakeCase)

  /**
    * Mapping function for field or class names that should be in snake_case format.
    *
    * @param s the name to transform
    * @return a transformed name or the same name if no transformation is required
    */
  private[this] def enforceSnakeCase(s: String): String = {
    val len = s.length
    val sb = new StringBuilder(len << 1)
    var i = 0
    var isPrecedingLowerCased = false
    while (i < len) isPrecedingLowerCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_') {
        sb.append('_')
        false
      } else if (Character.isLowerCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingLowerCased || i > 1 && i < len && Character.isLowerCase(s.charAt(i))) sb.append('_')
        sb.append(Character.toLowerCase(ch))
        false
      }
    }
    sb.toString
  }
}

object SnakeCaseJsonSupport extends SnakeCaseJsonSupport
