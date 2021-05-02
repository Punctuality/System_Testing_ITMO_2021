package ru.ifmo.se.software.testing.lab3

import cats.effect.Sync
import org.openqa.selenium._
import org.openqa.selenium.support.PageFactory

class LogicCase[F[_]: Sync](val driver: WebDriver) {

  lazy val loginButton: WebElement =
    driver findElement By.xpath("/html/body/div[1]/div/div/div/div/button")

  private val dialogPrefix = "/html/body/div[3]/div/div[2]/div/div/div[2]/div/div/div"

  lazy val login: WebElement =
    driver findElement By.xpath(s"$dialogPrefix/form/div[3]/div/div/div/div/input")

  lazy val password: WebElement =
    driver findElement By.xpath(s"$dialogPrefix/form[2]/div[2]/div/div/div/div/input")

  lazy val loginName: WebElement =
    driver findElement By.xpath(s"/html/body/div[1]/div/div[2]/div/div[1]/a[1]")

  lazy val loginFailureMessage: WebElement =
    driver findElement By.xpath(s"$dialogPrefix/div/div/div[2]")

  lazy val profilePickButton: WebElement =
    driver findElement By.xpath("/html/body/div[3]/div/div[2]/div/div/div/div/section/div[1]/div")

  def loginProcedure(credentials: LogicCase.Credentials): F[Unit] = Sync[F].delay {
    loginButton.click()
    login.sendKeys(credentials.login + Keys.ENTER)
    password.sendKeys(credentials.password + Keys.ENTER)
  }

  def pickProfileProcedure: F[Unit] = Sync[F].delay {
    profilePickButton.click()
  }

  def signOutProcedure(): F[Unit] = ???

  PageFactory.initElements(driver, this)
}

object LogicCase {
  case class Credentials(login: String, password: String)
}
