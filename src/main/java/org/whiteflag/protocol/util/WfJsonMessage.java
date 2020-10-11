/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.util;

import java.util.Map;
import com.github.cliftonlabs.json_simple.JsonObject;
import com.github.cliftonlabs.json_simple.JsonException;
import com.github.cliftonlabs.json_simple.Jsoner;

/**
 * Whiteflag JSON message representation
 */
public class WfJsonMessage extends JsonObject {

    /* PROPERTIES */

    /**
     * Serial version UID
     */
    private static final long serialVersionUID = 1L;

    /* Main object names i.a.w. WF JSON Message Specification */
    private static final String OBJECT_META = "MetaHeader";
    private static final String OBJECT_HEADER = "MessageHeader";
    private static final String OBJECT_BODY = "MessageBody"; 

    /* CONSTRUCTOR */

    public WfJsonMessage(Map<String, String> meta,
                         Map<String, String> header,
                         Map<String, String> body) {
        super();
        this.put(OBJECT_META, new JsonObject(meta));
        this.put(OBJECT_HEADER, new JsonObject(header));
        this.put(OBJECT_BODY, new JsonObject(body));
    }

    public WfJsonMessage(String jsonMessageStr) throws JsonException {
        super((JsonObject) Jsoner.deserialize(jsonMessageStr));
    }
}
