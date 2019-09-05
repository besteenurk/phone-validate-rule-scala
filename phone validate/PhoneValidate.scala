
class PhoneValidateRule extends StateFilter {
  override def executeFilter(
                              newItem: DLMRowItem,
                              historyItems: Seq[DLMRowItem],
                              filterMatchList: Seq[DLMRowItem],
                              matchDetail: MatchDetails
                            ): (Boolean, Seq[DLMRowItem], MatchDetails) = {

    val matched = isMatched(newItem)

    if (matched) {
      matchDetail.closable = true
    } else {
      matchDetail.closed = true
    }

    (
      matched,
      if (matched) filterMatchList :+ newItem else filterMatchList,
      matchDetail
    )

  }

  override def isMatched(newItem: DLMRowItem): Boolean = {

    val turkcell: List[String] = List("530", "531", "532", "533", "534", "535", "536", "537", "539", "561")
    val turkTelekom: List[String] = List("501", "505", "506", "507", "551", "552", "553", "554", "555", "559")
    val vodafone: List[String] = List("540", "541", "542", "543", "544", "545", "546", "547", "548", "549")

    val operatorMap = mutable.Map[String, List[String]]("turkcell" -> turkcell, "turkTelekom" -> turkTelekom, "vodafone" -> vodafone)
    var phone = newItem.get("phone")

    if (phone.startsWith("0")) {
      phone = phone.substring(1, 4)
    } else {
      phone = phone.substring(0, 3)
    }

    val e = phone.equals(operatorMap.keys)
    val operatorValues = new util.ArrayList[String]()

    for (value <- operatorMap.values) {
      value.foreach((e) => operatorValues.add(e))
    }

    if (operatorValues.contains(phone)) {
      for (elem <- operatorMap) {
        if (elem._2.contains(phone)) {
          if (elem._1.equals("turkcell")) {
            return true
          } else if (elem._1.equals("turkTelekom")) {
            return true
          } else if (elem._1.equals("vodafone")) {
            return true
          }
        }
      }
    } else {
      return false
    }
    e
  }
}
