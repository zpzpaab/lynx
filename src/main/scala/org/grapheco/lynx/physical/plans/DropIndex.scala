package org.grapheco.lynx.physical.plans

import org.grapheco.lynx.types.{LTAny, LynxType}
import org.grapheco.lynx.dataframe.DataFrame
import org.grapheco.lynx.physical.PhysicalPlannerContext
import org.grapheco.lynx.runner.ExecutionContext

case class DropIndex(labelName: String, properties: List[String])(implicit val plannerContext: PhysicalPlannerContext) extends AbstractPhysicalPlan {

  override def execute(implicit ctx: ExecutionContext): DataFrame = {
    graphModel._helper.dropIndex(labelName, properties.toSet)
    DataFrame.empty
  }

  override val schema: Seq[(String, LynxType)] = {
    Seq("DropIndex" -> LTAny)
  }
}
