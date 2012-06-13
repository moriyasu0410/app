package utils
import play.Logger
import _root_.models.kvses.OptOut
import _root_.models.AdRequest
import _root_.models.kvses.Configs
import _root_.models.kvses.DisabledTargetAd

/**
 * 広告リクエスト単位の属性情報付加制御
 */
object AttributeTargetingChecker {
  /**
   * 属性情報付与付加判定を行う<br>
   * 以下の条件を満たす場合のみOpenXへのリクエストに属性情報（au属性・外部連携属性）を付加可能OKと判定する
   * 
   * <ul>
   * <li>ユーザ単位の属性情報付加OK→オプトイン (＝optoutスキーマに対応するxidのレコードが存在しないこと)
   * <li>媒体単位の属性情報付加OK (＝ad_target_disableスキーマに対応するauidのレコードが存在しないこと)
   * <li>システム単位の属性情報付加OK(＝Config.config_target_enabledがtrueであること)
   * </ul>
   * 
   * @param xid androidid or macaddress or LocalStrageUSerID)
   * @param auid aduitId
   * @return true:属性情報付加OK / false:属性情報付加NG 
   */
  def isTargetable(xid:Option[String],auid:Option[String]):Boolean = {
    var optIn = OptInOutChecker.isOptIn(xid);
    var adTarget = AdTargetingChecker.isTargetable(auid)
    var sysTarget = SystemTargetingChecker.isTargetable()
    return optIn && adTarget && sysTarget
  }
}

/**
 * ユーザ単位の属性情報付加制御
 */
object OptInOutChecker {
  /** 
   * オプトイン・アウトチェック.<br>
   * @param xid : ID
   * @return true:オプトイン / false:オプトアウト
   */
  def isOptIn(xid:Option[String]):Boolean = {
    OptOut(xid).get match {
      case Some(optOut) => {return false}
      case None => {return true}
    }
  }
}

/**
 * 媒体単位の属性情報付加制御
 */
object AdTargetingChecker {
  /**
   * 媒体単位の属性情報付加判定を行う
   * @param auid adunitId
   * @return true:属性情報付加OK / false:属性情報付加NG
   */
  def isTargetable(auid:Option[String]):Boolean = {
    var auids = auid.get.split(",")
    for (auid <- auids){
      DisabledTargetAd(auid = Some(auid)).get() match {
        //上記キーに対応する媒体情報設定があればfalseを返して処理終了
        case Some(value) => return false
        case None => 
      }
    }
    return true
  }
}

/**
 * システム単位の属性情報付加制御
 */
object SystemTargetingChecker {
  /**
   * システム単位の属性情報付加制御
   * @return true:属性情報付加OK / false:属性情報付加NG
   */
  def isTargetable():Boolean = {
    Configs.target_enabled.get() match {
      case Some("true") => {return true}
      case _ => {return false}
    }
  }
}