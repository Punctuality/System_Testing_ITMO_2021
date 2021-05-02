package ru.ifmo.se.software.testing.lab3

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.syntax.traverse._
import org.junit.Assert._
import org.junit._
import org.openqa.selenium._
import org.openqa.selenium.chrome.ChromeDriver

import java.time.Duration


class FilmsLandingCaseSpec {
  private implicit def unitToIO(action: => Unit): IO[Unit] = IO(action)

  private val filmsUrl: String = "https://www.ivi.ru/movies"
  private val driverRef = Ref[IO].of[WebDriver](null).unsafeRunSync()
  private val pageCaseRef = Ref[IO].of[FilmsLandingCase[IO]](null).unsafeRunSync()

  private val customFilterTargetLink = "https://www.ivi.ru/movies/filters/arthouse+biography+boeviki"

  @Before
  def setUp(): Unit = (for {
    _ <- unitToIO { System.setProperty("webdriver.chrome.driver", "src/test/resources/chromedriver")}
    driver <- driverRef.updateAndGet(_ => new ChromeDriver)
    _ <- pageCaseRef.updateAndGet(_ => new FilmsLandingCase[IO](filmsUrl, driver))
  } yield ()).unsafeRunSync()

  @After
  def tearDown(): Unit =
    driverRef.get.map(_.quit()).unsafeRunSync()

  @Test
  def tagsFilter(): Unit = (
      for {
        page <- pageCaseRef.get
        targets: List[(String, Int)] = page.tagSuggestionList.map(a => a.getAttribute("href")).zipWithIndex.take(4)
        results <- targets.traverse{
          case (targetLink, idx) => page.tagsFilteringProcedure(idx).map(targetLink -> _)
        }
      } yield assertArrayEquals("Should visit the same sites by tags",
        results.map(_._1).toArray.asInstanceOf[Array[AnyRef]],
        results.map(_._2).toArray.asInstanceOf[Array[AnyRef]]
      )
    ).unsafeRunSync()

  @Test
  def compilationsFilter(): Unit = (
    for {
      page <- pageCaseRef.get
      targets: List[(String, Int)] = page.compilationSuggestionList.map(a => a.getAttribute("href")).zipWithIndex
      results <- targets.traverse{
        case (targetLink, idx) => page.compilationsFilteringProcedure(idx).map(targetLink -> _)
      }
    } yield assertArrayEquals("Should visit the same sites by compilations",
      results.map(_._1).toArray.asInstanceOf[Array[AnyRef]],
      results.map(_._2).toArray.asInstanceOf[Array[AnyRef]]
    )
    ).unsafeRunSync()

  @Test
  def genresFilter(): Unit = (
    for {
      page <- pageCaseRef.get
      targets: List[(String, Int)] = page.genresSuggestionList.map(a => a.getAttribute("href")).zipWithIndex
      results <- targets.traverse{
        case (targetLink, idx) => page.genresFilteringProcedure(idx).map(targetLink -> _)
      }
    } yield assertArrayEquals("Should visit the same sites by filters",
      results.map(_._1).toArray.asInstanceOf[Array[AnyRef]],
      results.map(_._2).toArray.asInstanceOf[Array[AnyRef]]
    )
    ).unsafeRunSync()

  @Test
  def customFilter(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.customFilterProcedure(0 :: 1 :: 2 :: Nil)
    } yield assertEquals("https://www.ivi.ru/movies/filters/arthouse+biography+boeviki", page.driver.getCurrentUrl)
  ).unsafeRunSync()
}
