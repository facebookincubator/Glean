// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.glean.descriptors.exceptions;

import com.facebook.glean.IndexerContext;
import com.facebook.glean.descriptors.utils.LocUtils;
import com.sun.source.tree.Tree;
import javax.lang.model.element.Element;

public class DescriptorException extends RuntimeException {

  public DescriptorException(IndexerContext ic, Element element) {
    this(ic, element, "");
  }

  public DescriptorException(IndexerContext ic, Element element, String message) {
    this("\n" + message + "\n" + LocUtils.getLocationStringOfElement(ic, element));
  }

  public DescriptorException(IndexerContext ic, Tree tree) {
    this(ic, tree, "");
  }

  public DescriptorException(IndexerContext ic, Tree tree, String message) {
    this("\n" + message + "\n" + LocUtils.getLocationStringOfTree(ic, tree));
  }

  public DescriptorException(String message) {
    super(message);
  }
}
