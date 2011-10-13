LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := pixelzoo

LOCAL_C_INCLUDES := $(LOCAL_PATH)/../../../src
# $(error $(LOCAL_C_INCLUDES))

LOCAL_CPP_EXTENSION := .cpp

LOCAL_SRC_FILES := $(wildcard ../../../src/*.c)
# $(error $(LOCAL_SRC_FILES))

LOCAL_SHARED_LIBRARIES :=
LOCAL_STATIC_LIBRARIES :=

include $(BUILD_SHARED_LIBRARY)
