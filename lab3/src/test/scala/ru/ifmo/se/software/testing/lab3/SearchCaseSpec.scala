package ru.ifmo.se.software.testing.lab3

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.junit.Assert._
import org.junit._
import org.openqa.selenium._
import org.openqa.selenium.chrome.ChromeDriver

import java.time.Duration


class SearchCaseSpec {
  private implicit def unitToIO(action: => Unit): IO[Unit] = IO(action)

  private val searchUrl: String = "https://www.ivi.ru/search"
  private val driverRef = Ref[IO].of[WebDriver](null).unsafeRunSync()
  private val pageCaseRef = Ref[IO].of[SearchCase[IO]](null).unsafeRunSync()

  private val searchText = "Семнадцать мгновений"
  private val targetUrl = "https://www.ivi.ru/watch/17_mgnoveniy_vesny"
  private val mainPresets: Array[AnyRef] = List(
    "a" -> "Премьеры фильмов",
    "a" -> "Новинки подписки",
    "a" -> "Сериалы Amediateka",
    "a" -> "Высокий рейтинг"
  ).toArray

  @Before
  def setUp(): Unit = (for {
    _ <- unitToIO { System.setProperty("webdriver.chrome.driver", "src/test/resources/chromedriver")}
    driver <- driverRef.updateAndGet(_ => new ChromeDriver)
    _ <- pageCaseRef.updateAndGet(_ => new SearchCase[IO](searchUrl, driver))
  } yield ()).unsafeRunSync()

  @After
  def tearDown(): Unit =
    driverRef.get.map(_.quit()).unsafeRunSync()

  @Test
  def basicSearch(): Unit = (
      for {
        page <- pageCaseRef.get
        _ <- page.basicSearchProcedure(searchText)
      } yield assertEquals("Should've found correct page", targetUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def searchWithHints(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.hintsSearchProcedure(1, searchText)
    } yield assertEquals("Should've found correct page", targetUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def presetSearches(): Unit = (
    for{
      page <- pageCaseRef.get
      presets <- page.presetsRetrieveProcedure()
    } yield assertArrayEquals("Should contain main presets", mainPresets, presets.asInstanceOf[Array[AnyRef]])
  ).unsafeRunSync()

}
