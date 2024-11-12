module Avalonia.Controls

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Builder

type RelativePanel with
    static member create (children: IAttr<RelativePanel> list) =
        ViewBuilder.Create<RelativePanel>(children)

type Control with
    /// Align the top edge of the child control with the top edge of the panel.
    static member alignTopWithPanel<'t when 't :> Control>(value: bool) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<_>(RelativePanel.AlignTopWithPanelProperty, value, ValueNone)

    /// Attached to a child control to align the bottom edge of the child control with the bottom edge of the panel.
    static member alignBottomWithPanel<'t when 't :> Control>(value: bool) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<_>(RelativePanel.AlignBottomWithPanelProperty, value, ValueNone)

    /// Attached to a child control to align the left edge of the child control with the left edge of the panel.
    static member alignLeftWithPanel<'t when 't :> Control>(value: bool) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<_>(RelativePanel.AlignLeftWithPanelProperty, value, ValueNone)

    /// Attached to a child control to align the right edge
    static member alignRightWithPanel<'t when 't :> Control>(value: bool) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<_>(RelativePanel.AlignRightWithPanelProperty, value, ValueNone)

    /// Attached to a child control to align the horizontal center of the child control with the horizontal center of the panel.
    static member alignHorizontalCenterWithPanel<'t when 't :> Control>(value: bool) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<_>(RelativePanel.AlignHorizontalCenterWithPanelProperty, value, ValueNone)

    /// Attached to a child control to align the vertical center of the child control with the vertical center of the panel.
    static member alignVerticalCenterWithPanel<'t when 't :> Control>(value: bool) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<_>(RelativePanel.AlignVerticalCenterWithPanelProperty, value, ValueNone)

    /// Attached to a child control to align its top edge with the top edge of the named sibling.
    static member alignTopWith<'t when 't :> Control>(value: string) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<RelativePanel>(RelativePanel.AlignTopWithProperty, value, ValueNone)

    /// Attached to a child control to align its bottom edge with the bottom edge of the named sibling.
    static member alignBottomWith<'t when 't :> Control>(value: string) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<RelativePanel>(RelativePanel.AlignBottomWithProperty, value, ValueNone)

    /// Attached to a child control to align its left edge with the left edge of the named sibling.
    static member alignLeftWith<'t when 't :> Control>(value: string) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<RelativePanel>(RelativePanel.AlignLeftWithProperty, value, ValueNone)

    /// Attached to a child control to align its right edge with the right edge of the named sibling.
    static member alignRightWith<'t when 't :> Control>(value: string) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<RelativePanel>(RelativePanel.AlignRightWithProperty, value, ValueNone)

    /// Attached to a child control to align its horizontal center with the horizontal center of the named sibling.
    static member alignHorizontalCenterWith<'t when 't :> Control>(value: string) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<RelativePanel>(RelativePanel.AlignHorizontalCenterWithProperty, value, ValueNone)

    /// Attached to a child control to align its vertical center with the vertical center of the named sibling.
    static member alignVerticalCenterWith<'t when 't :> Control>(value: string) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<RelativePanel>(RelativePanel.AlignVerticalCenterWithProperty, value, ValueNone)

    /// Attached to a child control to align its bottom edge with the top edge of the named sibling.
    static member above<'t when 't :> Control>(value: string) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<RelativePanel>(RelativePanel.AboveProperty, value, ValueNone)

    /// Attached to a child control to align its top edge with the bottom edge of the named sibling.
    static member below<'t when 't :> Control>(value: string) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<RelativePanel>(RelativePanel.BelowProperty, value, ValueNone)

    /// Attached to a child control to align its right edge with the left edge of the named sibling.
    static member leftOf<'t when 't :> Control>(value: string) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<RelativePanel>(RelativePanel.LeftOfProperty, value, ValueNone)

    /// Attached to a child control to align its left edge with the right edge of the named sibling.
    static member rightOf<'t when 't :> Control>(value: string) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<RelativePanel>(RelativePanel.RightOfProperty, value, ValueNone)