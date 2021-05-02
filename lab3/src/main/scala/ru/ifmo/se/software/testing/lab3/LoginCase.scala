package ru.ifmo.se.software.testing.lab3

import cats.effect.Sync
import org.openqa.selenium._
import org.openqa.selenium.support.PageFactory

import scala.util.Try

class LoginCase[F[_]: Sync](val driver: WebDriver) {

  lazy val loginButton: WebElement =
    driver findElement By.xpath("/html/body/div[1]/div/div/div/div/button")

  private val dialogPrefix = "/html/body/div[3]/div/div[2]/div/div/div[2]/div/div/div"

  def login: WebElement =
    driver findElement By.xpath(s"$dialogPrefix/form/div[3]/div/div/div/div/input")

  def password: WebElement =
    driver findElement By.xpath(s"$dialogPrefix/form[2]/div[2]/div/div/div/div/input")

  def loginName: WebElement =
    driver findElement By.xpath(s"/html/body/div[1]/div/div[2]/div/div[1]/a[1]")

  def loginFailureMessage: WebElement =
    driver findElement By.xpath(s"$dialogPrefix/div/div/div[2]")

  def profilePickButton: WebElement =
    driver findElement By.xpath("/html/body/div[3]/div/div[2]/div/div/div/div/section/div[1]/div")

  def profileHintCloseButton: WebElement =
    driver findElement By.xpath("/html/body/div[3]/div/div/div/div/div[2]/div")

  def signOutButton: WebElement =
    driver findElement By.xpath("/html/body/div[1]/div/section/div/div/div[2]/a[2]")

  def loginProcedure(credentials: LoginCase.Credentials): F[Unit] = Sync[F].delay {
    loginButton.click()
    login.sendKeys(credentials.login + Keys.ENTER)
    password.sendKeys(credentials.password + Keys.ENTER)
  }

  def pickProfileProcedure(): F[Unit] = Sync[F].delay {
    profilePickButton.click()
    profileHintCloseButton.click()
  }

  def signOutProcedure(): F[Unit] = Sync[F].delay{
    signOutButton.click()
  }

  PageFactory.initElements(driver, this)
}

object LoginCase {
  case class Credentials(login: String, password: String)
}
