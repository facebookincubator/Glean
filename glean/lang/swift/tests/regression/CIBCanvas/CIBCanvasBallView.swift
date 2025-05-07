// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

import UIKit

private let kRainbowColors: [UIColor] = [
  .lightGray,
  .red,
  .orange,
  .yellow,
  .green,
  .blue,
  .purple,
]

private let kTotalTransformations = kRainbowColors.count
private let kTotalDuration = TimeInterval(kTotalTransformations) * 0.2
private let kRelativeDuration = 1.0 / Double(kTotalTransformations)
private let kBallSize = CGSize(width: 40, height: 40)

final class CIBCanvasBallView: UIView {

  let ballId: String
  let label = UILabel()

  init(ballId: String) {
    self.ballId = ballId
    super.init(frame: CGRect(origin: .zero, size: kBallSize))
    layer.cornerRadius = kBallSize.width / 2
    label.textColor = .white
    addSubview(label)
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override public func layoutSubviews() {
    super.layoutSubviews()
    label.sizeToFit()
    label.frame = CGRect(
      x: bounds.minX + (bounds.width - label.frame.width) / 2,
      y: bounds.minY + (bounds.height - label.frame.height) / 2,
      width: label.frame.width,
      height: label.frame.height
    )
  }

  func update(altitude: CGFloat, color: UIColor, supportsUpdate: Bool) {
    alpha = supportsUpdate ? 0.7 : 0.4
    layer.borderWidth = supportsUpdate ? 2.0 : 0.0
    layer.borderColor = supportsUpdate ? UIColor.black.cgColor : UIColor.clear.cgColor
    backgroundColor = color
    label.text = altitude > 0 ? String(format: "%.1f", altitude) : ""
    setNeedsLayout()
  }

  func performRainbowAnimation(_ completion: @escaping () -> Void) {
    UIView.animateKeyframes(
      withDuration: kTotalDuration,
      delay: 0,
      options: [.calculationModeLinear, .allowUserInteraction]
    ) {
      for index in 0..<kTotalTransformations {
        UIView.addKeyframe(
          withRelativeStartTime: Double(index) * kRelativeDuration,
          relativeDuration: kRelativeDuration
        ) {
          self.backgroundColor = kRainbowColors[index % kRainbowColors.count]
        }
      }
    } completion: { _ in
      completion()
    }
  }
}
