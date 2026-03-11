#include <Yoga.h>

#include <stdlib.h>
#include <string.h>

inline YGValue* YGValueCopy(YGValue value) {
    YGValue* ret = malloc(sizeof(YGValue));
    memcpy(ret, &value, sizeof(YGValue));

    return ret;
}

void YGValueFree(YGValue* value) {
    free(value);
}

YGValue* YGNodeStyleGetFlexBasisWrapper(YGNodeRef node) {
    return YGValueCopy(YGNodeStyleGetFlexBasis(node));
}
YGValue* YGNodeStyleGetPositionWrapper(YGNodeRef node, YGEdge edge) {
    return YGValueCopy(YGNodeStyleGetPosition(node, edge));
}
YGValue* YGNodeStyleGetMarginWrapper(YGNodeRef node, YGEdge edge) {
    return YGValueCopy(YGNodeStyleGetMargin(node, edge));
}
YGValue* YGNodeStyleGetPaddingWrapper(YGNodeRef node, YGEdge edge) {
    return YGValueCopy(YGNodeStyleGetPadding(node, edge));
}
YGValue* YGNodeStyleGetWidthWrapper(YGNodeRef node) {
    return YGValueCopy(YGNodeStyleGetWidth(node));
}
YGValue* YGNodeStyleGetHeightWrapper(YGNodeRef node) {
    return YGValueCopy(YGNodeStyleGetHeight(node));
}
YGValue* YGNodeStyleGetMinWidthWrapper(YGNodeRef node) {
    return YGValueCopy(YGNodeStyleGetMinWidth(node));
}
YGValue* YGNodeStyleGetMinHeightWrapper(YGNodeRef node) {
    return YGValueCopy(YGNodeStyleGetMinHeight(node));
}
YGValue* YGNodeStyleGetMaxWidthWrapper(YGNodeRef node) {
    return YGValueCopy(YGNodeStyleGetMaxWidth(node));
}
YGValue* YGNodeStyleGetMaxHeightWrapper(YGNodeRef node) {
    return YGValueCopy(YGNodeStyleGetMaxHeight(node));
}
