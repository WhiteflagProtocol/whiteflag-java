/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.util;

import org.junit.Test;
import static org.junit.Assert.*;

/* Field encodings required for field definitions */
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;

/**
 * Whiteflag JSON validator utility test class
 */
public class WfJsonValidatorTest {

    /**
     * Static JSON mapper
     */
    private static final ObjectMapper mapper = new ObjectMapper();
    static {
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    /**
     * Tests JSON message validation against JSON message schema
     */
    @Test
    public void testJsonMessageValidation1() throws JsonProcessingException {
        /* Create JSON nodes for valid Free Text and Sign/Signal message */
        JsonNode jsonMessageNode1 = mapper.readTree("{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}");
        JsonNode jsonMessageNode2 = mapper.readTree("{\"MetaHeader\":{\"blockchain\":\"ethereum\",\"originatorAddress\":\"129536af7e25f9b3a777f72d222f811fbc607fcb\"},\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"S\",\"ReferenceIndicator\":\"0\",\"ReferencedMessage\":\"4c1b8331ba07cf644d4d5065f5c954fe1370e3b0fa0cab37f3b87261f153673d\"},\"MessageBody\":{\"SubjectCode\":\"10\",\"DateTime\":\"2018-01-01T00:00:00Z\",\"Duration\":\"P00D00H00M\",\"ObjectType\":\"11\",\"ObjectLatitude\":\"+50.00000\",\"ObjectLongitude\":\"+005.00000\",\"ObjectSizeDim1\":\"0000\",\"ObjectSizeDim2\":\"0000\",\"ObjectOrientation\":\"000\"}}");
        /* Verify */
        assertTrue("JSON message should validate against JSON message schema", WfJsonValidator.validateNode(jsonMessageNode1));
        assertTrue("JSON message with metadata should validate against JSON message schema", WfJsonValidator.validateNode(jsonMessageNode2));
    }
    /**
     * Tests JSON message validation against JSON message schema
     */
    @Test
    public void testJsonMessageValidation2() throws JsonProcessingException {
        /* Create JSON nodes for invalid Free Text message */
        JsonNode jsonMessageNode1 = mapper.readTree("{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"3\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}");
        JsonNode jsonMessageNode2 = mapper.readTree("{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"P\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}");
        JsonNode jsonMessageNode3 = mapper.readTree("{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"X\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}");

        /* Verify */
        assertFalse("JSON message should NOT validate against JSON message schema (invalid DuressIndicator)", WfJsonValidator.validateNode(jsonMessageNode1));
        assertFalse("JSON message should NOT validate against JSON message schema (invalid MessageBody)", WfJsonValidator.validateNode(jsonMessageNode2));
        assertFalse("JSON message should NOT validate against JSON message schema (invalid MessageCode)", WfJsonValidator.validateNode(jsonMessageNode3));
    }
    /**
     * Tests JSON message validation against JSON message schema
     */
    @Test
    public void testJsonMessageValidation3() throws JsonProcessingException {
        /* Create JSON node for invalid Sign/Signal message */
        JsonNode jsonMessageNode1 = mapper.readTree("{\"MetaHeader\":{\"blockchain\":\"ethereum\",\"originatorAddress\":\"129536af7e25f9b3a777f72d222f811fbc607fcb\"},\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"S\",\"ReferenceIndicator\":\"0\",\"ReferencedMessage\":\"4c1b8331ba07cf644d4d5065f5c954fe1370e3b0fa0cab37f3b87261f153673d\"},\"MessageBody\":{\"SubjectCode\":\"10\",\"DateTime\":\"2018-01-01T00:00:00Z\",\"ObjectType\":\"11\",\"ObjectLatitude\":\"+50.00000\",\"ObjectLongitude\":\"+005.00000\",\"ObjectSizeDim1\":\"0000\",\"ObjectSizeDim2\":\"0000\",\"ObjectOrientation\":\"000\"}}");

        /* Verify */
        assertFalse("JSON message should NOT validate against JSON message schema (missing Duration field)", WfJsonValidator.validateNode(jsonMessageNode1));
    }
}