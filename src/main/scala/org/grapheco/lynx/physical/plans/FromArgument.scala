package org.grapheco.lynx.physical.plans

import org.grapheco.lynx.types.{LTNode, LynxType}
import org.grapheco.lynx.dataframe.DataFrame
import org.grapheco.lynx.physical.PhysicalPlannerContext
import org.grapheco.lynx.runner.ExecutionContext

case class FromArgument(str: String)(implicit val plannerContext: PhysicalPlannerContext) extends LeafPhysicalPlan {

  override val schema: Seq[(String, LynxType)] = Seq((str, LTNode)) // Fixme: hard code

  override def execute(implicit ctx: ExecutionContext): DataFrame = {
    ctx.arguments.select(Seq((str, None)))
  }
}
