package smegments

sealed trait ResourceState
case object Meta extends ResourceState
case object Summary extends ResourceState
case object Detailed extends ResourceState
case object Unknown extends ResourceState

object ResourceState {

  def apply(stateId: Int): ResourceState = stateId match {
    case 1 => Meta
    case 2 => Summary
    case 3 => Detailed
    case _ => Unknown
  }
}