// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

import UIKit

@objc public protocol CIBCanvasViewDelegate: NSObjectProtocol {
  @objc func canvasView(_: CIBCanvasView, didTapBallId: String)
  @objc func canvasView(_: CIBCanvasView, didTapPosition: CGPoint)
  @objc func canvasView(_: CIBCanvasView, didDoubleTapPosition: CGPoint)
  @objc func canvasView(_: CIBCanvasView, didTripleTapPosition: CGPoint)
  @objc func canvasView(_: CIBCanvasView, didDoubleTapBallId: String)
  @objc func canvasView(_: CIBCanvasView, didTripleTapBallId: String)
  @objc func canvasView(_: CIBCanvasView, didLongPressBallId: String)
  @objc func canvasView(_: CIBCanvasView, didPanBallId: String, position: CGPoint)
}

@objc
public final class CIBCanvasView: UIView {
  var views: [String: CIBCanvasBallView] = [:]

  @objc public var delegate: CIBCanvasViewDelegate?

  override public init(frame: CGRect) {
    super.init(frame: frame)
    backgroundColor = UIColor(white: 0.9, alpha: 0.9)
    layer.borderWidth = 1
    layer.borderColor = UIColor.gray.cgColor
    isMultipleTouchEnabled = true
    layer.masksToBounds = true

    let tripleTap = UITapGestureRecognizer(target: self, action: #selector(canvasTripleTapped))
    tripleTap.numberOfTapsRequired = 3
    addGestureRecognizer(tripleTap)
    let doubleTap = UITapGestureRecognizer(target: self, action: #selector(canvasDoubleTapped))
    doubleTap.numberOfTapsRequired = 2
    doubleTap.require(toFail: tripleTap)
    addGestureRecognizer(doubleTap)
    let tap = UITapGestureRecognizer(target: self, action: #selector(canvasTapped))
    tap.numberOfTapsRequired = 1
    tap.require(toFail: doubleTap)
    addGestureRecognizer(tap)
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  @objc public func addOrUpdateBall(
    withId ballId: String,
    position: CGPoint,
    altitude: CGFloat,
    color: UIColor,
    supportsUpdate: Bool
  ) {
    let ballView = createBallViewIfNeeded(ballId)
    ballView.update(altitude: altitude, color: color, supportsUpdate: supportsUpdate)
    ballView.center = position
  }

  @objc public func removeBall(withId ballId: String) {
    views[ballId]?.removeFromSuperview()
    views[ballId] = nil
  }

  @objc public func removeAll() {
    for (_, view) in views {
      view.removeFromSuperview()
    }
    views.removeAll()
  }

  @objc public func performRainbowAnimation(withId ballId: String, completion: @escaping () -> Void) {
    if let ballView = views[ballId] {
      ballView.performRainbowAnimation(completion)
    }
  }

  private func createBallViewIfNeeded(_ ballId: String) -> CIBCanvasBallView {
    if let ballView = views[ballId] {
      return ballView
    }

    let ballView = CIBCanvasBallView(ballId: ballId)
    addSubview(ballView)
    views[ballId] = ballView

    let pan = UIPanGestureRecognizer(target: self, action: #selector(ballPanned))
    ballView.addGestureRecognizer(pan)

    let tripleTap = UITapGestureRecognizer(target: self, action: #selector(ballTripleTapped))
    tripleTap.numberOfTapsRequired = 3
    ballView.addGestureRecognizer(tripleTap)

    let doubleTap = UITapGestureRecognizer(target: self, action: #selector(ballDoubleTapped))
    doubleTap.require(toFail: tripleTap)
    doubleTap.numberOfTapsRequired = 2
    ballView.addGestureRecognizer(doubleTap)

    let tap = UITapGestureRecognizer(target: self, action: #selector(ballTapped))
    tap.require(toFail: doubleTap)
    ballView.addGestureRecognizer(tap)

    let longPress = UILongPressGestureRecognizer(target: self, action: #selector(ballLongPressed))
    ballView.addGestureRecognizer(longPress)

    return ballView
  }
}

// MARK: - Gesture Handlers

extension CIBCanvasView {

  @objc private func ballPanned(sender: UIPanGestureRecognizer) {
    if let ballView = sender.view as? CIBCanvasBallView,
      let delegate
    {
      let location = sender.location(in: self)
      if bounds.contains(location) {
        delegate.canvasView(self, didPanBallId: ballView.ballId, position: location)
      }
    }
  }

  @objc private func ballTapped(sender: UITapGestureRecognizer) {
    if let ballView = sender.view as? CIBCanvasBallView,
      let delegate
    {
      delegate.canvasView(self, didTapBallId: ballView.ballId)
    }
  }

  @objc private func ballDoubleTapped(sender: UITapGestureRecognizer) {
    if let ballView = sender.view as? CIBCanvasBallView,
      let delegate
    {
      delegate.canvasView(self, didDoubleTapBallId: ballView.ballId)
    }
  }

  @objc private func ballTripleTapped(sender: UITapGestureRecognizer) {
    if let ballView = sender.view as? CIBCanvasBallView,
      let delegate
    {
      delegate.canvasView(self, didTripleTapBallId: ballView.ballId)
    }
  }

  @objc private func ballLongPressed(sender: UILongPressGestureRecognizer) {
    if let ballView = sender.view as? CIBCanvasBallView,
      let delegate
    {
      switch sender.state {
      case .began, .changed:
        delegate.canvasView(self, didLongPressBallId: ballView.ballId)
      case .ended, .cancelled, .failed:
        break
      default:
        break
      }
    }
  }

  @objc private func canvasTapped(sender: UITapGestureRecognizer) {
    if let delegate {
      let location = sender.location(in: self)
      if bounds.contains(location) {
        delegate.canvasView(self, didTapPosition: sender.location(in: self))
      }
    }
  }

  @objc private func canvasDoubleTapped(sender: UITapGestureRecognizer) {
    if let delegate {
      let location = sender.location(in: self)
      if bounds.contains(location) {
        delegate.canvasView(self, didDoubleTapPosition: sender.location(in: self))
      }
    }
  }

  @objc private func canvasTripleTapped(sender: UITapGestureRecognizer) {
    if let delegate {
      let location = sender.location(in: self)
      if bounds.contains(location) {
        delegate.canvasView(self, didTripleTapPosition: sender.location(in: self))
      }
    }
  }
}
