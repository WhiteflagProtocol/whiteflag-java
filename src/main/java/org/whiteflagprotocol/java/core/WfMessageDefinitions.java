/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

/* Field encodings required for field definitions */
import static org.whiteflagprotocol.java.core.WfMessageField.Encoding.*;

/**
 * Whiteflag message definitions utility class
 *
 * <p> This is a non-instantiatable utility class that holds all message and
 * message field definitions in accordance with the Whiteflag specification.
 * No implementation specific properties and methods are defined by this class.
 * 
 * @wfver v1-draft.6
 * @wfref 4. Message Format
 */
public class WfMessageDefinitions {

    /* CONSTRUCTOR */

    /** 
     * Prevents the utility class to be instantiated
     */
    private WfMessageDefinitions() {
        throw new IllegalStateException("Cannot instantiate Whiteflag message definitions utility class");
    }

    /* FIELDS */

    /**
     *  Undefined fields
     */
    protected static final WfMessageField[] UNDEFINED = new WfMessageField[] {};

    /**
     * Generic message header fields
     * @wfver v1-draft.6
     * @wfref 4.2.1 Generic Message Header
     */
    protected static final WfMessageField[] genericHeaderFields = new WfMessageField[] {
        new WfMessageField("Prefix", "^WF$", UTF8, 0, 2),
        new WfMessageField("Version", "(?=1)^[A-Z0-9]{1}$", UTF8, 2, 3),
        new WfMessageField("EncryptionIndicator", "(?=0|1|2)^[A-Z0-9]{1}$", UTF8, 3, 4),
        new WfMessageField("DuressIndicator", "^[0-1]{1}$", BIN, 4, 5),
        new WfMessageField("MessageCode", "(?=A|K|T|P|E|S|D|I|M|Q|R|F)^[A-Z]{1}$", UTF8, 5, 6),
        new WfMessageField("ReferenceIndicator", "(?=0|1|2|3|4|5|6|7|8|9)^"+HEX.charset()+"{1}$", HEX, 6, 7),
        new WfMessageField("ReferencedMessage", "^"+HEX.charset()+"{64}$", HEX, 7, 71)
    }; 

    /**
     * Authentication message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.4 Management Messages: Authentication
     */
    protected static final WfMessageField[] authenticationBodyFields = new WfMessageField[] {
        new WfMessageField("VerificationMethod", "(?=1|2)^"+HEX.charset()+"{1}$", HEX, 71, 72),
        new WfMessageField("VerificationData", "^"+UTF8.charset()+"*$", UTF8, 72, -1)
    };

    /**
     * Cryptographic message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.5 Management Messages: Cryptographic Support
     */
    protected static final WfMessageField[] cryptoBodyFields = new WfMessageField[] {
        new WfMessageField("CryptoDataType", "^"+HEX.charset()+"{2}$", HEX, 71, 73),
        new WfMessageField("CryptoData", "^"+HEX.charset()+"*$", HEX, 73, -1)
    };

    /**
     * Free text message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.3 Functional Messages: Free Text
     */
    protected static final WfMessageField[] freetextBodyFields = new WfMessageField[] {
        new WfMessageField("Text", "^"+UTF8.charset()+"*$", UTF8, 71, -1)
    };

    /**
     * Resource message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.2 Functional Messages: Resource
     */
    protected static final WfMessageField[] resourceBodyFields = new WfMessageField[] {
        new WfMessageField("ResourceMethod", "(?=1)^"+HEX.charset()+"{1}$", HEX, 71, 72),
        new WfMessageField("ResourceData", "^"+UTF8.charset()+"*$", UTF8, 72, -1)
    };

    /**
     * Test message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.6 Management Messages: Test
     */
    protected static final WfMessageField[] testBodyFields = new WfMessageField[] {
        new WfMessageField("PseudoMessageCode", "^[A-Z]{1}$", UTF8, 71, 72)
    };

    /**
     * Sign/signal message body fields
     * @wfver v1-draft.6
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     */
    protected static final WfMessageField[] signsignalBodyFields = new WfMessageField[] {
        new WfMessageField("SubjectCode", "^"+HEX.charset()+"{2}$", HEX, 71, 73),
        new WfMessageField("DateTime", "^"+DATETIME.charset()+"$", DATETIME, 73, 93),
        new WfMessageField("Duration", "^"+DURATION.charset()+"$", DURATION, 93, 103),
        new WfMessageField("ObjectType", "^"+HEX.charset()+"{2}$", HEX, 103, 105),
        new WfMessageField("ObjectLatitude", "^"+LAT.charset()+"$", LAT, 105, 114),
        new WfMessageField("ObjectLongitude", "^"+LONG.charset()+"$", LONG, 114, 124),
        new WfMessageField("ObjectSizeDim1", "^"+DEC.charset()+"{4}$", DEC, 124, 128),
        new WfMessageField("ObjectSizeDim2", "^"+DEC.charset()+"{4}$", DEC, 128, 132),
        new WfMessageField("ObjectOrientation", "^"+DEC.charset()+"{3}$", DEC, 132, 135)
    };

    /**
     * Request fields
     * @wfver v1-draft.6
     * @wfref 4.3.1.9 Object Request Fields
     */
    protected static final WfMessageField[] requestFields = new WfMessageField[] {
        new WfMessageField("ObjectType", "^"+HEX.charset()+"{2}$", HEX, 135, 137),
        new WfMessageField("ObjectTypeQuant", "^"+DEC.charset()+"{2}$", DEC, 137, 139)
    };
}
